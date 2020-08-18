#lang racket/base

(provide run-openssl-command
         (struct-out exn:fail:xiden:openssl))

(require racket/file
         racket/function
         racket/path
         racket/port
         racket/system
         "contract.rkt"
         "query.rkt"
         "rc.rkt"
         "string.rkt"
         "url.rkt")

(define openssl (find-executable-path "openssl"))
(struct exn:fail:xiden:openssl exn:fail (exit-code))

(define (run-openssl-command stdin-source . args)
  (define-values (sp from-stdout to-stdin from-stderr)
    (apply subprocess #f #f #f openssl args))

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
                    (raise (exn:fail:xiden:openssl
                            (format "OpenSSL failed with exit code ~a: ~a"
                                    exit-code
                                    error-string)
                            (current-continuation-marks)
                            exit-code)))

                  (define output (port->bytes from-stdout))
                  output)
                (λ ()
                  (close-input-port from-stderr)
                  (close-input-port from-stdout))))
