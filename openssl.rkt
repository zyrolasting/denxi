#lang racket/base

(require racket/format
         racket/port
         "contract.rkt"
         "exn.rkt")

(provide run-openssl-command
         openssl
         (struct-out exn:fail:xiden:openssl)
         (contract-out
          [md-algorithms
           (non-empty-listof symbol?)]
          [md-algorithm/c
           flat-contract?]
          [md-bytes-source/c
           flat-contract?]
          [make-digest
           (-> md-bytes-source/c
               md-algorithm/c
               bytes?)]))

(define-exn exn:fail:xiden:openssl exn:fail:xiden (exit-code output))

(define openssl (find-executable-path "openssl"))

(define md-algorithms
  '(md4
    md5
    sha1
    sha224
    sha256
    sha3-224
    sha3-256
    sha3-384
    sha3-512
    sha384
    sha512
    sha512-224
    sha512-256))


(define md-bytes-source/c
  (or/c path-string? bytes? input-port?))


(define md-algorithm/c
  (apply or/c md-algorithms))


(define (make-digest variant algorithm)
  (cond [(path-string? variant)
         (call-with-input-file (expand-user-path variant)
           (λ (i) (make-digest i algorithm)))]
        [(bytes? variant)
         (make-digest (open-input-bytes variant) algorithm)]
        [(input-port? variant)
         (run-openssl-command variant "dgst" "-binary" (~a "-" algorithm))]))


(define (run-openssl-command #:timeout [delay-seconds 3] stdin-source . args)
  (define-values (sp from-stdout to-stdin from-stderr)
    (apply subprocess #f #f #f (and (subprocess-group-enabled) 'new) openssl args))

  (copy-port stdin-source to-stdin)
  (flush-output to-stdin)
  (close-output-port to-stdin)

  (dynamic-wind void
                (λ ()
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

                  (define output (port->bytes from-stdout))

                  (unless (eq? exit-code 0)
                    (raise ((exc exn:fail:xiden:openssl exit-code output)
                            "OpenSSL failed with exit code ~a: ~a"
                            exit-code
                            error-string)))

                  output)
                (λ ()
                  (close-input-port from-stderr)
                  (close-input-port from-stdout))))
