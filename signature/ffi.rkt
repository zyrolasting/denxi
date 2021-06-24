#lang racket/base

(require racket/contract)
(provide
 (contract-out
  [signature-ffi-available?! (-> boolean?)]
  [signature-ffi-get-end-signature! (-> any/c)]
  [signature-ffi-get-find-signature-size! (-> any/c)]
  [signature-ffi-get-make-md-context! (-> any/c)]
  [signature-ffi-get-start-signature! (-> any/c)]
  [signature-ffi-get-verify-signature! (-> any/c)]
  [signature-ffi-make-signature! (->* (bytes? symbol? bytes?) ((or/c #f bytes?)) bytes?)]
  [signature-ffi-verify-signature! (-> bytes? symbol? bytes? bytes? boolean?)]))

  
(require (rename-in ffi/unsafe [-> -->])
         "../integrity/ffi.rkt"
         "../crypto.rkt")


(define-syntax-rule (define-ffi-accessor id expr)
  (begin (define (id) expr)
         (hash-set! private-table 'id id)))


(define private-table
  (make-hash))


(define (signature-ffi-verify-signature! digest
                                         algorithm
                                         signature-bytes
                                         public-key-bytes)
  (define p-md (integrity-ffi-load-chf! algorithm))
  (define digest-size ((integrity-ffi-get-get-digest-size!) p-md))
  (define verify (signature-ffi-get-verify-signature!))
  (define status
    (verify p-md
            signature-bytes
            (bytes-length signature-bytes)
            public-key-bytes
            digest
            digest-size))
  (if (< status 0)
      (crypto-raise!)
      (equal? status 1)))


(define (signature-ffi-make-signature! digest
                                       algorithm
                                       private-key-bytes
                                       [private-key-password-bytes #f])
  (define get-digest-size (integrity-ffi-get-get-digest-size!))
  (define make-md-context (signature-ffi-get-make-md-context!))
  (define start-signature (signature-ffi-get-start-signature!))
  (define find-signature-size (signature-ffi-get-find-signature-size!))
  (define end-signature (signature-ffi-get-end-signature!))

  (define p-md (integrity-ffi-load-chf! algorithm))
  (define digest-size (get-digest-size p-md))
  (define p-ctx (make-md-context))

  (start-signature p-md
                   p-ctx
                   private-key-bytes
                   private-key-password-bytes
                   digest
                   digest-size)

  (define signature-size
    (find-signature-size p-ctx))

  (unless (> signature-size 0)
    (crypto-raise!))

  (define signature
    (make-bytes signature-size))

  (end-signature p-ctx
                 signature
                 signature-size)

  signature)


(define (signature-ffi-available?!)
  (and (for/and ([(k v) (in-hash private-table)]) v)
       #t))


(define-ffi-accessor signature-ffi-get-start-signature!
  (crypto-get-obj! #"xiden_start_signature"
                   (_fun _pointer ; EVP_MD* p_md,
                         _pointer ; EVP_MD_CTX* p_md_ctx,
                         _bytes/nul-terminated  ; char* p_private_key_content,
                         _bytes/nul-terminated  ; char* p_private_key_password,
                         _bytes/nul-terminated ; char* p_digest,
                         _size    ; size_t digest_length
                         --> _int)))


(define-ffi-accessor signature-ffi-get-find-signature-size!
  (crypto-get-obj! #"xiden_find_signature_size"
                   (_fun _pointer ; EVP_MD_CTX* p_md_ctx
                         --> _size)))


(define-ffi-accessor signature-ffi-get-end-signature!
  (crypto-get-obj! #"xiden_end_signature"
                   (_fun _pointer ; EVP_MD_CTX* p_ctx
                         _pointer ; char* p_signature
                         _uint    ; size_t signature_length
                         --> _int)))


(define-ffi-accessor signature-ffi-get-verify-signature!
  (crypto-get-obj! #"xiden_verify_signature"
                   (_fun _pointer ; EVP_MD* md,
                         _bytes/nul-terminated ; char* pSignature,
                         _uint    ; size_t signatureLength
                         _bytes/nul-terminated ; char* pPublicKeyContent,
                         _bytes/nul-terminated ; char* pDigest,
                         _uint    ; size_t digestLength
                         --> _int)))


(define-ffi-accessor signature-ffi-get-make-md-context!
  (crypto-get-obj! #"EVP_MD_CTX_new"
                   (_fun --> _pointer)))

(module+ test
  (require rackunit
           "snake-oil.rkt"
           "../codec.rkt"
           "../integrity/ffi.rkt"
           (submod "../integrity/chf.rkt" test))
  (when (and (signature-ffi-available?!)
             (integrity-ffi-available?!))

    (define available-chfs (map car test-digests))

    (for ([pair (in-list test-digests)])
      (define chf (car pair))
      (define expected-digest (base64 (cdr pair)))
      (with-handlers ([$crypto:error? (λ ($) (fail))])
        (define signature
          (signature-ffi-make-signature!
           expected-digest
           chf
           snake-oil-private-key
           snake-oil-private-key-password))

        (define (valid? #:expected-digest [e expected-digest]
                        #:chf [c chf]
                        #:signature [s signature]
                        #:public-key [p snake-oil-public-key])
          (signature-ffi-verify-signature! e c s p))

        (define (tamper bstr)
          (define copy (bytes-copy bstr))
          (bytes-set! copy 0 (modulo (add1 (bytes-ref bstr 0)) 255))
          copy)

        (test-true "Create and verify signature"
                   (valid?))

        (test-false "Reject signatures with the wrong CHF"
                    (valid? #:chf (findf (λ (v) (not (equal? v chf)))
                                         available-chfs)))

        (test-false "Reject doctored signatures"
                    (valid? #:signature (tamper signature)))

        (test-false "Reject signatures against the wrong digest"
                    (valid? #:expected-digest (tamper expected-digest)))))))
