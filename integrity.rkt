#lang racket/base

(require "contract.rkt")

(define zcpkg-hash-algorithms
  '(sha384 sha512 sha1 md5))

(define zcpkg-hash-source/c
  (or/c path-string? bytes? input-port?))

(define zcpkg-hash-algorithm/c
  (apply or/c zcpkg-hash-algorithms))

(provide (struct-out zcpkg-integrity-info)
         (contract-out
          [zcpkg-hash-algorithms
           (non-empty-listof symbol?)]
          [zcpkg-hash-algorithm/c
           flat-contract?]
          [alist->zcpkg-integrity-info
           (-> list? zcpkg-integrity-info?)]
          [make-digest
           (-> zcpkg-hash-source/c
               zcpkg-hash-algorithm/c
               bytes?)]
          [zcpkg-integrity-check
           (-> zcpkg-integrity-info?
               zcpkg-hash-source/c
               boolean?)]))

(require racket/sequence
         racket/format
         "encode.rkt"
         "file.rkt"
         "list.rkt"
         "verify.rkt"
         "zcpkg-settings.rkt")

(struct zcpkg-integrity-info (algorithm digest) #:prefab)

(define (alist->zcpkg-integrity-info l)
  (zcpkg-integrity-info
   (assoc+ 'algorithm l)
   (assoc+ 'digest l)))

(define (make-zcpkg-integrity-info variant algorithm)
  (zcpkg-integrity-info algorithm (make-digest variant algorithm)))

(define (make-digest variant algorithm)
  (cond [(path-string? variant)
         (call-with-input-file variant (λ (i) (make-digest i algorithm)))]
        [(bytes? variant)
         (make-digest (open-input-bytes variant) algorithm)]
        [(input-port? variant)
         (define-values (exit-code digest)
           (run-openssl-command variant
                                "dgst"
                                "-binary"
                                (~a "-" algorithm)))

         (if (eq? exit-code 0)
             digest
             (error "Cannot make digest for ~s: OpenSSL returned exit code ~a"
                    variant
                    exit-code))]))


(define (zcpkg-integrity-check info variant)
  (or (equal? (zcpkg-integrity-info-digest info)
              (make-digest (zcpkg-integrity-info-algorithm info) variant))
      (ZCPKG_TRUST_BAD_DIGEST)))


(module+ test
  (require rackunit)

  (test-equal? "Declare integrity info using list"
               (alist->zcpkg-integrity-info
                '((algorithm sha384)
                  (digest #"abc")))
               (zcpkg-integrity-info 'sha384 #"abc"))

  (test-equal? "Declare integrity info using empty list"
               (alist->zcpkg-integrity-info null)
               (zcpkg-integrity-info #f #f))

  (test-case "Create integrity information"
    (for ([algorithm (in-list zcpkg-hash-algorithms)])
      (define bstr (string->bytes/utf-8 (symbol->string algorithm)))
      (define info (make-zcpkg-integrity-info bstr algorithm))
      (check-pred zcpkg-integrity-info? info)
      (check-eq? (zcpkg-integrity-info-algorithm info) algorithm)
      (check-equal? (zcpkg-integrity-info-digest info)
                    (make-digest bstr algorithm)))))
