#lang racket/base

; This module acts as an inflection point between a privileged runtime
; and a less-privileged runtime.  This is not a substitute for
; OS-level security.

(require (only-in net/url-connect
                  current-https-protocol)
         racket/contract
         racket/exn
         racket/function
         racket/list
         openssl
         "codec.rkt"
         "crypto.rkt"
         "integrity.rkt"
         "state.rkt"
         "message.rkt"
         "path.rkt"
         "port.rkt"
         "setting.rkt")

(provide restrict)

(define+provide-message $restrict (name))
(define+provide-message $restrict:operation $restrict (reporting-guard summary args))
(define+provide-message $restrict:budget $restrict (kind amount))

(define+provide-setting XIDEN_ALLOW_ENV (listof (or/c bytes-environment-variable-name? string?)) null)
(define+provide-setting XIDEN_TRUST_EXECUTABLES (listof well-formed-integrity-info/c) null)
(define+provide-setting XIDEN_TRUST_ANY_EXECUTABLE boolean? #f)
(define+provide-setting XIDEN_TRUST_CERTIFICATES (listof path-string?) null)
(define+provide-setting XIDEN_TRUST_HOST_EXECUTABLES (listof string?) null)
(define+provide-setting XIDEN_TRUST_UNVERIFIED_HOST boolean? #f)
(define+provide-setting XIDEN_MEMORY_LIMIT_MB (>=/c 0) 200)
(define+provide-setting XIDEN_TIME_LIMIT_S (>=/c 0) (* 5 60))


(define (restrict halt
                  proc
                  #:memory-limit memory-limit
                  #:time-limit time-limit
                  #:trusted-executables trusted-executables
                  #:allowed-envvars allowed-envvars
                  #:trust-unverified-host? trust-unverified-host?
                  #:trust-any-executable? trust-any-executable?
                  #:implicitly-trusted-host-executables implicitly-trusted-host-executables
                  #:trust-certificates trust-certificates
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
                                                   trust-unverified-host?
                                                   trust-certificates
                                                   proc)
                               (λ (worker-thread)
                                 (unless (sync/timeout time-limit worker-thread)
                                   (plan 1 ($restrict:budget name 'time time-limit)))
                                 (finish))))))


;-------------------------------------------------------------------------------
; Control

(define (make-worker-thread name
                            memory-limit
                            security-guard
                            envvars plan
                            trust-unverified-host?
                            trust-certificates
                            proc)
  (thread
   (λ ()
     (call-with-custom-custodian memory-limit
                                 (λ ()
                                   (with-handlers ([exn:fail:out-of-memory?
                                                    (λ _ (plan 1 ($restrict:budget name 'space memory-limit)))]
                                                   [exn?
                                                    (λ (e) (plan 1 ($show-string (exn->string e))))])
                                     (parameterize ([current-environment-variables envvars]
                                                    [current-https-protocol
                                                     (make-ssl-context trust-certificates
                                                                       trust-unverified-host?)]
                                                    [current-security-guard security-guard])
                                       (call-with-values (λ () (call/cc proc))
                                                         plan))))))))


(define (make-ssl-context trust-certificates trust-unverified-host?)
  (define ctx (ssl-make-client-context 'auto))
  (unless trust-unverified-host?
    (ssl-load-default-verify-sources! ctx)
    (for ([source (in-list trust-certificates)])
      (ssl-load-verify-source! ctx source))
    (ssl-set-verify! ctx #t)
    (ssl-set-verify-hostname! ctx #t)
    ; No weak cipher suites
    (ssl-set-ciphers! ctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
    ; Seal context so further changes cannot weaken it
    (ssl-seal-context! ctx)
    ctx))

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
                                    #:workspace [ws (XIDEN_WORKSPACE)])
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
  (λ (op link-path maybe-complete-target-path)
    (define target-path (simple-form-path maybe-complete-target-path))
    (unless (path-prefix? target-path workspace)
      (raise ($restrict:operation name
                                  'link
                                  'blocked-link
                                  (list op link-path target-path))))))


(define (get-writeable-workspace-directories [wd (XIDEN_WORKSPACE)])
  (list (current-directory)
        wd))



(module+ test
  (require racket/match
           racket/runtime-path
           rackunit
           "file.rkt")

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

  (test-case "Prescribe write directories per-workspace"
    (define writeables (get-writeable-workspace-directories))
    (check-pred (non-empty-listof complete-path?) writeables)
    (check-equal? (get-writeable-workspace-directories (XIDEN_WORKSPACE))
                  writeables))

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
    (call-with-snake-oil-chf-profile
     (λ ()
       (define my-file "security.rkt")
       (define my-path (build-path here my-file))
       (define other-path (build-path here "main.rkt"))

       (define trusts-anything
         (make-executable-trust-predicate #t '() '()))

       (define trusts-nothing
         (make-executable-trust-predicate #f '() '()))

       (define trusts-exact-things
         (make-executable-trust-predicate #f
                                          (list (integrity 'snake-oil
                                                           (make-digest my-path 'snake-oil)))
                                          '()))

       (define trusts-hosted-things
         (make-executable-trust-predicate #f '() '("security.rkt") (λ (p) my-path)))

       (check-true  (trusts-anything my-path))
       (check-true  (trusts-anything other-path))

       (check-false (trusts-nothing my-path))
       (check-false (trusts-nothing other-path))

       (check-true  (trusts-exact-things my-path))
       (check-false (trusts-exact-things other-path))

       (check-true  (trusts-hosted-things my-path))
       (check-false (trusts-hosted-things other-path)))))


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
