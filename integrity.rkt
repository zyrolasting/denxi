#lang racket/base

(require "contract.rkt")

(define zcpkg-hash-algorithms
  '(sha384 sha512 sha1 md5))

(define zcpkg-hash-encodings
  '(base64 base32 b32 b64))

(define zcpkg-hash-algorithm/c
  (apply or/c zcpkg-hash-algorithms))

(define zcpkg-hash-encoding/c
  (apply or/c zcpkg-hash-encodings))

(provide (struct-out zcpkg-integrity-info)
         (contract-out
          [zcpkg-hash-algorithms
           (non-empty-listof symbol?)]
          [zcpkg-hash-encodings
           (non-empty-listof symbol?)]
          [zcpkg-hash-algorithm/c
           flat-contract?]
          [zcpkg-hash-encoding/c
           flat-contract?]
          [alist->zcpkg-integrity-info
           (-> list? zcpkg-integrity-info?)]
          [encode-digest
           (-> zcpkg-hash-encoding/c bytes? bytes?)]
          [decode-digest
           (-> zcpkg-hash-encoding/c bytes? bytes?)]
          [make-digest
           (-> (or/c path-string? bytes? input-port?)
               zcpkg-hash-algorithm/c
               bytes?)]))

(require racket/sequence
         racket/format
         net/base64
         base32
         "file.rkt"
         "list.rkt"
         "verify.rkt")

(struct zcpkg-integrity-info (algorithm encoding body) #:prefab)

(define (alist->zcpkg-integrity-info l)
  (zcpkg-integrity-info
   (assoc+ 'algorithm l)
   (assoc+ 'encoding l)
   (assoc+ 'body l)))

(define (make-zcpkg-integrity-info variant algorithm encoding)
  (zcpkg-integrity-info algorithm
                        encoding
                        (encode-digest encoding (make-digest variant algorithm))))

; Contracts will raise error when misused
(define (encode-digest encoding digest)
  (case encoding
    [(base32 b32) (base32-encode-bytes digest)]
    [(base64 b64) (base64-encode digest #"")]))

(define (decode-digest encoding encoded-digest)
  (case encoding
    [(base32 b32) (base32-decode-bytes encoded-digest)]
    [(base64 b64) (base64-decode encoded-digest)]))

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


(module+ test
  (require rackunit)

  (test-equal? "Declare integrity info using list"
               (alist->zcpkg-integrity-info
                '((algorithm sha384)
                  (encoding b32)
                  (body #"abc")))
               (zcpkg-integrity-info 'sha384 'b32 #"abc"))

  (test-equal? "Declare integrity info using empty list"
               (alist->zcpkg-integrity-info null)
               (zcpkg-integrity-info #f #f #f))

  (test-case "Create integrity information"
    (for* ([algorithm (in-list zcpkg-hash-algorithms)]
           [encoding (in-list '(base64 base32 b32 b64))])
      (define bstr (string->bytes/utf-8 (symbol->string algorithm)))
      (define info (make-zcpkg-integrity-info bstr algorithm encoding))
      (check-pred zcpkg-integrity-info? info)
      (check-eq? (zcpkg-integrity-info-algorithm info) algorithm)
      (check-eq? (zcpkg-integrity-info-encoding info) encoding)
      (check-equal? (zcpkg-integrity-info-body info)
                    (encode-digest encoding (make-digest bstr algorithm)))
      (check-equal? (decode-digest encoding (zcpkg-integrity-info-body info))
                    (make-digest bstr algorithm)))))
