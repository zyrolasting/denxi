#lang racket/base

(require racket/contract
         racket/format
         racket/port
         "message.rkt"
         "setting.rkt")

(provide run-openssl-command
         openssl
         (contract-out
          [cryptographic-hash-functions
           (non-empty-listof symbol?)]
          [chf/c
           flat-contract?]
          [DEFAULT_CHF
            chf/c]
          [md-bytes-source/c
           flat-contract?]
          [make-digest
           (->* (md-bytes-source/c)
                (chf/c)
                bytes?)]))

(define+provide-message $openssl-error (args timeout exit-code output reason))

(define openssl (find-executable-path "openssl"))

(define cryptographic-hash-functions
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


(define chf/c
  (apply or/c cryptographic-hash-functions))


(define+provide-setting XIDEN_TRUST_CHFS (listof chf/c) null)


(define DEFAULT_CHF 'sha3-384)


(define (make-digest variant [algorithm DEFAULT_CHF])
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

                  (define error-info
                    (and maybe-sp
                         (port->bytes from-stderr)))

                  (define output (port->bytes from-stdout))

                  (unless (eq? exit-code 0)
                    (raise ($openssl-error args
                                           (or maybe-sp
                                               delay-seconds)
                                           exit-code
                                           output
                                           error-info)))

                  output)
                (λ ()
                  (close-input-port from-stderr)
                  (close-input-port from-stdout))))
