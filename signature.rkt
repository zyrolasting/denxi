#lang racket/base

(require "contract.rkt")

(define xiden-cipher-algorithms
  '(rsa))

(define xiden-cipher-algorithm/c
  (apply or/c xiden-cipher-algorithms))

(provide (struct-out signature-info)
         (contract-out
          [make-signature
           (-> bytes? path-string? bytes?)]
          [well-formed-signature-info/c
           flat-contract?]
          [xiden-cipher-algorithms
           (non-empty-listof symbol?)]
          [xiden-cipher-algorithm/c
           flat-contract?]
          [check-signature
           (-> bytes? bytes? bytes? boolean?)]))

(require racket/sequence
         racket/format
         racket/port
         "file.rkt"
         "integrity.rkt"
         "rc.rkt"
         "openssl.rkt")

(struct signature-info (algorithm pubkey body) #:prefab)


(define (well-formed-signature-info/c info)
  (struct/c signature-info
            xiden-cipher-algorithm/c
            bytes?
            bytes?))


(define (make-signature digest private-key-path)
  (run-openssl-command (open-input-bytes digest)
                       "pkeyutl"
                       "-sign"
                       "-inkey" private-key-path))

(define (check-signature digest signature public-key)
  (define tmpsig (make-temporary-file))
  (call-with-output-file tmpsig
    #:exists 'truncate/replace
    (λ (o) (copy-port (open-input-bytes signature) o)))
  (define-values (exit-code msg)
    (run-openssl-command
     (open-input-bytes digest)
     "pkeyutl"
     "-verify"
     "-sigfile" tmpsig
     "-pubin" "-inkey" public-key))
  (eq? exit-code 0))
