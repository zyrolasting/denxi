#lang racket/base

; This module acts as an inflection point between a privileged runtime
; and a less-privileged runtime.  This is not a substitute for
; OS-level security.

(require (only-in net/url-connect
                  current-https-protocol)
         racket/exn
         racket/function
         racket/list
         "codec.rkt"
         "integrity.rkt"
         "message.rkt"
         "openssl.rkt"
         "path.rkt"
         "port.rkt"
         "workspace.rkt")

(provide restrict)

(define+provide-message $restrict (name))
(define+provide-message $restrict:operation $restrict (reporting-guard summary args))
(define+provide-message $restrict:budget $restrict (kind amount))

(define (restrict halt
                  proc
                  #:memory-limit memory-limit
                  #:time-limit time-limit
                  #:trusted-executables trusted-executables
                  #:allowed-envvars allowed-envvars
                  #:trust-unverified-host? trust-unverified-host?
                  #:trust-any-executable? trust-any-executable?
                  #:implicitly-trusted-host-executables implicitly-trusted-host-executables
                  #:workspace workspace
                  #:gc-period gc-period
                  #:name [name (or (object-name proc) "")])
  (call-with-managed-thread (make-gc-thread gc-period)
   (λ _
     ; The `plan' mechanic allows the two threads at play
     ; to decide how to apply `halt'.
     (define finish halt)
     (define (plan c m)
       (set! finish (λ () (halt c m))))

     (define security-guard
       (make-custom-security-guard
        #:name name
        #:trust-any-executable? trust-any-executable?
        #:trust-executables trusted-executables
        #:trust-host-executables implicitly-trusted-host-executables
        #:workspace workspace))

     (call-with-managed-thread
      (make-worker-thread name
                          memory-limit
                          security-guard
                          (make-envvar-subset allowed-envvars)
                          plan
                          (if trust-unverified-host? 'auto 'secure)
                          proc)
      (λ (worker-thread)
        (unless (sync/timeout time-limit worker-thread)
          (plan 1 ($restrict:budget name 'time time-limit)))
        (finish))))))


;-------------------------------------------------------------------------------
; Control

(define (make-worker-thread name memory-limit security-guard envvars plan https-protocol proc)
  (thread
   (λ ()
     (call-with-custom-custodian memory-limit
      (λ ()
        (with-handlers ([exn:fail:out-of-memory?
                         (λ _ (plan 1 ($restrict:budget name 'space memory-limit)))]
                        [exn?
                         (λ (e) (plan 1 ($show-string (exn->string e))))])
          (parameterize ([current-environment-variables envvars]
                         [current-https-protocol https-protocol]
                         [current-security-guard security-guard])
            (call-with-values (λ () (call/cc proc))
                              plan))))))))


;-------------------------------------------------------------------------------
; Memory management

(define (make-custom-custodian memory-limit-mb)
  (let ([stop-cust (make-custodian)])
    (if (or (equal? memory-limit-mb +inf.0)
            (not (custodian-memory-accounting-available?)))
        stop-cust
        (begin (custodian-limit-memory stop-cust
                                       (mebibytes->bytes memory-limit-mb)
                                       stop-cust)
               stop-cust))))

(define (call-with-custom-custodian memory-limit-mb proc)
  (let ([cust (make-custom-custodian memory-limit-mb)])
    (dynamic-wind void
                  (λ ()
                    (parameterize ([current-custodian cust])
                      (proc)))
                  (λ ()
                    (custodian-shutdown-all cust)))))

(define (make-gc-thread period)
  (thread
   (λ ()
     (let loop ()
       (sync/timeout period never-evt)
       (collect-garbage)
       (loop)))))

(define (call-with-managed-thread th proc)
  (dynamic-wind void
                (λ () (proc th))
                (λ () (kill-thread th))))


;-------------------------------------------------------------------------------
; Environment variables

(define (make-envvar-subset allowed [input-set (current-environment-variables)])
  (define subset-names (remove-duplicates (map coerce-bytes (cons "PATH" allowed))))
  (apply make-environment-variables
         (for/fold ([mappings null])
                   ([name (in-list subset-names)])
           (define value (environment-variables-ref input-set name))
           (if value
               (cons name
                     (cons (environment-variables-ref input-set name)
                           mappings))
               mappings))))



;-------------------------------------------------------------------------------
; Security guard

(define (make-custom-security-guard #:name name
                                    #:trust-any-executable? trust-any-executable?
                                    #:trust-executables trust-executables
                                    #:trust-host-executables trust-host-executables
                                    #:workspace [ws (workspace-directory)])
  (make-security-guard
   (current-security-guard)
   (make-file-guard #:trust-any-executable? trust-any-executable?
                    #:trust-host-executables trust-host-executables
                    #:trust-executables trust-executables
                    #:writeable-directories (get-writeable-workspace-directories ws)
                    name)
   (make-network-guard name)
   (make-link-guard name ws)))



(define (make-file-guard #:trust-any-executable? trust-any-executable?
                         #:trust-host-executables trust-host-executables
                         #:trust-executables trust-executables
                         #:writeable-directories write-dirs
                         name)
  (let ([trust-executable? (make-executable-trust-predicate trust-any-executable?
                                                            trust-executables
                                                            (cons "openssl" trust-host-executables))])
    (λ (sym path-or-#f ops)
      (define (check-destructive-op op path)
        (define test (curry path-prefix? (normalize-path path)))
        (unless (ormap test write-dirs)
          (raise ($restrict:operation name 'file op (list sym path-or-#f ops)))))

      (when path-or-#f
        (cond [(member 'execute ops)
               (unless (trust-executable? path-or-#f)
                 (raise ($restrict:operation name 'file 'blocked-execute (list sym path-or-#f ops))))]

              [(member 'write ops)
               (check-destructive-op 'blocked-write path-or-#f)]

              [(member 'delete ops)
               (check-destructive-op 'blocked-delete path-or-#f)])))))


(define (make-executable-trust-predicate trust-any-executable? trust-executables trust-host-executables [find find-executable-path])
  (define trust-by-integrity? (bind-trust-list trust-executables))
  (define host-executable-paths (map normalize-path (filter-map find trust-host-executables)))
  (λ (path)
    (let ([normalized (normalize-path path)])
      (or trust-any-executable?
          (and (member normalized host-executable-paths) #t)
          (trust-by-integrity? normalized)))))


(define (make-network-guard name)
  (λ (sym hostname-or-#f port-or-#f client-or-server)
    (unless hostname-or-#f
      (raise ($restrict:operation name
                                  'network
                                  'blocked-listen
                                  (list sym hostname-or-#f port-or-#f client-or-server))))))


(define (make-link-guard name workspace)
  (λ (op link-path target-path)
    (define simple (simplify-path (path->complete-path target-path workspace)))
    (unless (path-prefix? simple workspace)
      (raise ($restrict:operation name
                                  'link
                                  'blocked-link
                                  (list op link-path target-path))))))


(define (get-writeable-workspace-directories [wd (workspace-directory)])
  (list (current-directory)
        (build-path wd "var/xiden")
        (build-path wd "tmp")))



(module+ test
  (require racket/match
           racket/runtime-path
           rackunit
           "file.rkt"
           "strict-rc.rkt")

  (define-runtime-path here ".")

  (define (station-security-guard f n l continue)
    (parameterize ([current-security-guard
                    (make-security-guard (current-security-guard) f n l)])
      (continue)))

  (define (station-file-guard f p)
    (station-security-guard f void void p))

  (define (station-network-guard n p)
    (station-security-guard void n void p))

  (define (station-link-guard l p)
    (station-security-guard void void l p))

  (test-case "Terminate thread at end of procedure"
    (define useless (thread (λ () (sync never-evt))))
    (call-with-managed-thread useless
                              (λ (th)
                                (check-eq? th useless)
                                (check-pred thread-running? th)))
    (check-pred thread-dead? useless))

  (test-exn "Restrict listening for network connections"
            (λ (e)
              (match e
                [($restrict:operation "_" 'network 'blocked-listen _) #t]
                [_ #f]))
            (λ ()
              (station-network-guard (make-network-guard "_")
                                     (λ ()
                                       (local-require racket/tcp)
                                       (tcp-listen 9000)))))

  (test-case "Restrict link creation to files in workspaces"
    (call-with-temporary-directory
     (λ (dir)
       (station-link-guard
        (make-link-guard "_" dir)
        (λ ()
          (define link-path (build-path dir "link"))
          (define target-path (build-path dir "target"))
          (define (ln t)
            (make-file-or-directory-link t link-path))

          ; The security guard only objects if a symlink points
          ; outside to a non-child of `dir`.
          (check-exn
           (λ (e)
             (match e
               [($restrict:operation "_" 'link 'blocked-link _) #t]
               [_ #f]))
           (λ ()
             ; Using two 'up arguments because the guard sees only
             ; a trailing slash difference for some reason.
             (ln (build-path target-path 'up 'up))))

          (check-not-exn
           (λ () (ln target-path))))))))


  (test-case "Build executable trust profiles"
    (define my-file "security.rkt")
    (define my-path (build-path here my-file))
    (define other-path (build-path here "main.rkt"))

    (define trusts-anything
      (make-executable-trust-predicate #t '() '()))

    (define trusts-nothing
      (make-executable-trust-predicate #f '() '()))

    (define trusts-exact-things
      (make-executable-trust-predicate #f
                                       (list (integrity 'sha1 (make-digest my-path 'sha1)))
                                       '()))

    (define trusts-hosted-things
      (make-executable-trust-predicate #f '() '("security.rkt") (λ (p) my-path)))

    (check-true  (trusts-anything my-path))
    (check-true  (trusts-anything other-path))

    (check-false (trusts-nothing my-path))
    (check-false (trusts-nothing other-path))

    (rc-rebind 'XIDEN_TRUST_MESSAGE_DIGEST_ALGORITHMS
               '(sha1)
               (λ ()
                 (check-true  (trusts-exact-things my-path))
                 (check-false (trusts-exact-things other-path))))

    (check-true  (trusts-hosted-things my-path))
    (check-false (trusts-hosted-things other-path)))


  (test-case "Make envvar subset"
    (define subset
      (make-envvar-subset
       '(#"bar")
       (make-environment-variables #"foo" #"1"
                                   #"bar" #"2"
                                   #"baz" #"3")))

    (check-false (environment-variables-ref subset #"foo"))
    (check-false (environment-variables-ref subset #"baz"))
    (check-equal? (environment-variables-ref subset #"bar") #"2")))
