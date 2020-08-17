#lang racket/base

(require "contract.rkt")

(define zcpkg-cipher-algorithms
  '(rsa))

(define zcpkg-cipher-algorithm/c
  (apply or/c zcpkg-cipher-algorithms))

(provide (struct-out zcpkg-signature-info)
         (contract-out
          [zcpkg-cipher-algorithms
           (non-empty-listof symbol?)]
          [zcpkg-cipher-algorithm/c
           flat-contract?]
          [zcpkg-signature-check
           (-> zcpkg-integrity-info? zcpkg-signature-info? boolean?)]
          [alist->zcpkg-signature-info
           (-> list? zcpkg-integrity-info?)]))

(require racket/sequence
         racket/format
         racket/port
         "file.rkt"
         "integrity.rkt"
         "list.rkt"
         "verify.rkt"
         "zcpkg-settings.rkt")

(struct zcpkg-signature-info (algorithm pubkey body) #:prefab)


(define (alist->zcpkg-signature-info l)
  (zcpkg-signature-info
   (assoc+ 'algorithm l)
   (assoc+ 'pubkey l) ; TODO: The public key probably won't be declared. Reference a keyring?
   (assoc+ 'body l)))


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

(define (zcpkg-signature-check integrity-info signature-info)
  (define signature (zcpkg-signature-info-body signature-info))
  (if signature
      (or (verify-signature (zcpkg-integrity-info-digest integrity-info)
                            signature
                            (zcpkg-signature-info-pubkey signature-info))
          (ZCPKG_TRUST_BAD_SIGNATURE))
      (or (ZCPKG_TRUST_BAD_DIGEST)
          (ZCPKG_TRUST_UNSIGNED))))
