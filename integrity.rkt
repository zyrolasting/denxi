#lang racket/base

(require "contract.rkt")

; Mirror OpenSSL
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

(provide (struct-out integrity-info)
         (contract-out
          [integrity
           (-> md-algorithm/c
               bytes?
               integrity-info?)]
          [md-algorithms
           (non-empty-listof symbol?)]
          [md-algorithm/c
           flat-contract?]
          [md-bytes-source/c
           flat-contract?]
          [well-formed-integrity-info/c
           flat-contract?]
          [make-digest
           (-> md-bytes-source/c
               md-algorithm/c
               bytes?)]
          [passed-integrity-check?
           (-> $integrity?
               boolean?)]
          [check-integrity
           (-> #:trust-bad-digest any/c
               any/c
               md-bytes-source/c
               $integrity?)]))

(require racket/sequence
         racket/format
         "codec.rkt"
         "file.rkt"
         "message.rkt"
         "openssl.rkt")

(define+provide-message $integrity (algorithm status))

(struct integrity-info (algorithm digest) #:prefab)

(define integrity integrity-info)

(define (digest-length-ok? info)
  (equal? (bytes-length (integrity-info-digest info))
          (bytes-length (make-digest #"whatever"
                                     (integrity-info-algorithm info)))))



(define well-formed-integrity-info/c
  (and/c (struct/c integrity-info
                   md-algorithm/c
                   bytes?)
         digest-length-ok?))


(define (make-digest variant algorithm)
  (cond [(path-string? variant)
         (call-with-input-file variant (λ (i) (make-digest i algorithm)))]
        [(bytes? variant)
         (make-digest (open-input-bytes variant) algorithm)]
        [(input-port? variant)
         (run-openssl-command variant
                              "dgst"
                              "-binary"
                              (~a "-" algorithm))]
        [else (raise-argument-error 'make-digest
                                    "A path, bytes, or an input port"
                                    variant)]))


(define (passed-integrity-check? status)
  (and (member ($integrity-status status) '(trusted verified))
       #t))

(define (check-integrity #:trust-bad-digest trust-bad-digest intinfo variant)
  (let ([return (λ (v) ($integrity (integrity-info-algorithm intinfo) v))])
    (if trust-bad-digest
        (return 'trusted)
        (if (well-formed-integrity-info/c intinfo)
            (if (equal? (integrity-info-digest intinfo)
                        (make-digest variant (integrity-info-algorithm intinfo)))
                (return 'verified)
                (return 'mismatch))
            (return 'missing)))))


(module+ test
  (require rackunit)

  (test-case "Create integrity information"
    (for ([algorithm (in-list md-algorithms)])
      (define bstr (string->bytes/utf-8 (symbol->string algorithm)))
      (define info (integrity-info bstr algorithm))
      (check-pred integrity-info? info)
      (check-eq? (integrity-info-algorithm info) algorithm)
      (check-equal? (integrity-info-digest info)
                    (make-digest bstr algorithm)))))
