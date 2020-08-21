#lang racket/base

(require "contract.rkt")

(define xiden-hash-algorithms
  '(sha384 sha512 sha1 md5))

(define xiden-hash-source/c
  (or/c path-string? bytes? input-port?))

(define xiden-hash-algorithm/c
  (apply or/c xiden-hash-algorithms))

(provide (struct-out integrity-info)
         (contract-out
          [xiden-hash-algorithms
           (non-empty-listof symbol?)]
          [xiden-hash-algorithm/c
           flat-contract?]
          [well-formed-integrity-info?
           predicate/c]
          [make-digest
           (-> xiden-hash-source/c
               xiden-hash-algorithm/c
               bytes?)]
          [check-integrity
           (-> well-formed-integrity-info?
               xiden-hash-source/c
               boolean?)]))

(require racket/sequence
         racket/format
         "encode.rkt"
         "file.rkt"
         "rc.rkt"
         "openssl.rkt")

(struct integrity-info (algorithm digest) #:prefab)

(define (make-integrity-info variant algorithm)
  (integrity-info algorithm (make-digest variant algorithm)))

(define (well-formed-integrity-info? info)
  (and (integrity-info? info)
       (xiden-hash-algorithm/c (integrity-info-algorithm info))
       (bytes? (integrity-info-digest info))
       (equal? (bytes-length (integrity-info-digest info))
               (bytes-length (make-digest #"whatever"
                                          (integrity-info-algorithm info))))))


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

(define (check-integrity info variant)
  (equal? (integrity-info-digest info)
          (make-digest variant (integrity-info-algorithm info))))

(module+ test
  (require rackunit)

  (test-case "Create integrity information"
    (for ([algorithm (in-list xiden-hash-algorithms)])
      (define bstr (string->bytes/utf-8 (symbol->string algorithm)))
      (define info (make-integrity-info bstr algorithm))
      (check-pred integrity-info? info)
      (check-eq? (integrity-info-algorithm info) algorithm)
      (check-equal? (integrity-info-digest info)
                    (make-digest bstr algorithm)))))
