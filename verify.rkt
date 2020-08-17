#lang racket/base

; Define procedures used to verify custom data,
; so that other processes can continue.

(provide validate-zcpkg-info
         run-openssl-command
         make-error-message-accumulator
         (struct-out exn:fail:zcpkg:openssl))

(require racket/file
         racket/function
         racket/path
         racket/port
         racket/system
         "contract.rkt"
         "string.rkt"
         "url.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt"
         "zcpkg-query.rkt")

(define openssl (find-executable-path "openssl"))
(struct exn:fail:zcpkg:openssl exn:fail (exit-code))

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
                        (begin (subprocess-kill sp) 1)
                        (subprocess-status sp)))

                  (define error-string
                    (if maybe-sp
                        (port->string from-stderr)
                        (format "Command timed out after ~a seconds. zcpkg terminated the subprocess."
                                delay-seconds)))

                  (unless (eq? exit-code 0)
                    (raise (exn:fail:zcpkg:openssl
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


; TODO: Write hash algorithm-dependent check
(define (non-empty-bytes? b)
  (and (bytes? b)
       (> (bytes-length b) 0)))

(define (make-error-message-accumulator)
  (define errors null)
  (case-lambda [() (reverse errors)]
               [(v msg) (unless v (set! errors (cons msg errors)))]))

(define (validate-zcpkg-info info #:for-server? [for-server? #f])
  (define errors null)
  (define (proc->string p) (symbol->string (object-name p)))
  (define prefix-length (add1 (string-length (proc->string zcpkg-info))))

  (define (<< f . a) (set! errors (cons (apply format f a) errors)))
  (define (check predicate accessor expected)
    (unless (predicate (accessor info))
      (<< "~a: not ~a. Got: ~s"
          (substring (proc->string accessor) prefix-length)
          expected
          (accessor info))))

  (check name-string? zcpkg-info-provider-name
         "a valid name string")

  (check name-string? zcpkg-info-package-name
         "a valid name string")

  (check name-string? zcpkg-info-edition-name
         "a valid name string")

  (check revision-number? zcpkg-info-revision-number
         "an exact non-negative integer")

  (check (listof name-string?) zcpkg-info-revision-names
         "a list of name strings")

  (check (or/c #f path-string?)
         zcpkg-info-setup-module
         "a path string, or #f")

  (check (listof (or/c zcpkg-query-string?
                       path-string?
                       url-string?))
         zcpkg-info-dependencies
         "a list containing zcpkg-query URNs, URLs, or paths")

  (when for-server?
    (check non-empty-bytes? zcpkg-info-integrity
           "a byte string")

    (check non-empty-bytes? zcpkg-info-signature
           "a byte string"))

  (reverse errors))

(module+ test
  (require rackunit)

  (test-case "Accumulate error messages to report later"
    (define e (make-error-message-accumulator))
    (check-pred procedure? e)
    (check-equal? (e) null)
    (check-pred void? (e #t "something"))
    (check-equal? (e) null)
    (check-pred void? (e #f "something"))
    (check-pred void? (e #f "or"))
    (check-pred void? (e #f "other"))
    (check-equal? (e) '("something" "or" "other"))))
