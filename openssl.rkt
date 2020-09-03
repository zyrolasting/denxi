#lang racket/base

(provide run-openssl-command)

(require racket/file
         racket/function
         racket/path
         racket/port
         racket/system
         "contract.rkt"
         "exn.rkt"
         "query.rkt"
         "rc.rkt"
         "string.rkt"
         "url.rkt")

(define-exn exn:fail:xiden:openssl exn:fail:xiden (exit-code))

(define openssl (find-executable-path "openssl"))

(define (run-openssl-command stdin-source . args)
  (define-values (sp from-stdout to-stdin from-stderr)
    (apply subprocess #f #f #f (and (subprocess-group-enabled) 'new) openssl args))

  (copy-port stdin-source to-stdin)
  (flush-output to-stdin)
  (close-output-port to-stdin)

  (dynamic-wind void
                (λ ()
                  (define delay-seconds 3)
                  (define maybe-sp (sync/timeout delay-seconds sp))

                  (define exit-code
                    (if maybe-sp
                        (subprocess-status sp)
                        (begin (subprocess-kill sp #t) 1)))

                  (define error-string
                    (if maybe-sp
                        (port->string from-stderr)
                        (format "Command timed out after ~a seconds. xiden terminated the subprocess."
                                delay-seconds)))

                  (unless (eq? exit-code 0)
                    (raise ((exc exn:fail:xiden:openssl exit-code)
                            "OpenSSL failed with exit code ~a: ~a"
                            exit-code
                            error-string)))

                  (define output (port->bytes from-stdout))
                  output)
                (λ ()
                  (close-input-port from-stderr)
                  (close-input-port from-stdout))))
