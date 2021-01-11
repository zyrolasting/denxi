#lang racket/base

; This module acts as an inflection point between a privileged runtime
; and a less-privileged runtime.  This is not a substitute for
; OS-level security.

(require "message.rkt")
(provide restrict)
(define+provide-message $restrict (name))
(define+provide-message $restrict:operation $restrict (reporting-guard summary args))
(define+provide-message $restrict:budget $restrict (kind amount))

(require racket/exn
         racket/function
         racket/list
         (only-in net/url-connect current-https-protocol)
         "codec.rkt"
         "integrity.rkt"
         "logged.rkt"
         "message.rkt"
         "path.rkt"
         "port.rkt"
         "workspace.rkt")

(define (restrict halt
                  proc
                  #:memory-limit memory-limit
                  #:time-limit time-limit
                  #:trusted-executables trusted-executables
                  #:allowed-envvars allowed-envvars
                  #:trust-unverified-host? trust-unverified-host?
                  #:trust-any-executable? trust-any-executable?
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
                                    #:workspace [ws (workspace-directory)])
  (make-security-guard
   (current-security-guard)
   (make-file-guard #:trust-any-executable? trust-any-executable?
                    #:trust-executables trust-executables
                    #:writeable-directories (get-writeable-workspace-directories ws)
                    name)
   (make-network-guard name)
   (make-link-guard name ws)))


(define (make-file-guard #:trust-any-executable? trust-any-executable?
                         #:trust-executables trust-executables
                         #:writeable-directories write-dirs
                         name)
  (let ([trust-executable? (bind-trust-list trust-executables)])
    (λ (sym path-or-#f ops)
      (define (check-destructive-op op path)
        (define test (curry path-prefix? (normalize-path path)))
        (unless (ormap test write-dirs)
          (raise ($restrict:operation name 'file op (list sym path-or-#f ops)))))

      (when path-or-#f
        (cond [(member 'execute ops)
               (unless (or (equal? "openssl" (path->string (file-name-from-path path-or-#f)))
                           trust-any-executable?
                           (trust-executable? path-or-#f))
                 (raise ($restrict:operation name
                                             'file
                                             'blocked-execute
                                             (list sym path-or-#f ops))))]

              [(member 'write ops)
               (check-destructive-op 'blocked-write path-or-#f)]

              [(member 'delete ops)
               (check-destructive-op 'blocked-delete path-or-#f)])))))


(define (make-network-guard name)
  (λ (sym hostname-or-#f port-or-#f client-or-server)
    (unless hostname-or-#f
      (raise ($restrict:operation name
                                  'network
                                  'blocked-listen
                                  (list sym hostname-or-#f port-or-#f client-or-server))))))


(define (make-link-guard name workspace)
  (define (path-ok? p)
    (path-prefix? (simplify-path (if (complete-path? p) p (build-path workspace p)))
                  workspace))

  (λ (op link-path target-path)
    (unless (path-ok? (normalize-path target-path))
      (raise ($restrict:operation name
                                  'link
                                  'blocked-link
                                  (list op link-path target-path))))))


(define (get-writeable-workspace-directories [wd (workspace-directory)])
  (list (current-directory)
        (build-path wd "var/xiden")
        (build-path wd "tmp")))



(module+ test
  (require rackunit)

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
