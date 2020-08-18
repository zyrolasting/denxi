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
          [xiden-cipher-algorithms
           (non-empty-listof symbol?)]
          [xiden-cipher-algorithm/c
           flat-contract?]
          [check-signature
           (-> integrity-info? signature-info? boolean?)]))

(require racket/sequence
         racket/format
         racket/port
         "file.rkt"
         "integrity.rkt"
         "rc.rkt"
         "openssl.rkt")

(struct signature-info (algorithm pubkey body) #:prefab)

(define (make-signature digest private-key-path)
  (run-openssl-command (open-input-bytes digest)
                       "pkeyutl"
                       "-sign"
                       "-inkey" private-key-path))

(define (verify-signature digest signature public-key)
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

(define (check-signature int sig)
  (or (XIDEN_TRUST_BAD_DIGEST)
      (if (and sig (signature-info-body sig))
          (or (verify-signature (integrity-info-digest int) sig (signature-info-pubkey sig))
              (or (XIDEN_TRUST_BAD_SIGNATURE)
                  'mismatch))
          (or (XIDEN_TRUST_UNSIGNED)
              'missing))))

(define (verify-signature binfo)
  (define signature (bytes-info-signature binfo))
  (if signature
      (if (check-signature (bytes-info-integrity binfo) signature)
          'ok
          (raise ($bad-signature binfo)))
      (raise ($missing-signature binfo))))
