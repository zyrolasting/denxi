#lang racket/base

(require racket/async-channel
         racket/contract
         racket/string
         racket/unsafe/ops
         file/sha1
         net/base64
         "rfc4648.rkt")

(provide
 (contract-out
  [abbreviated-decode-procedure/c chaperone-contract?]
  [base32 abbreviated-decode-procedure/c]
  [base64 abbreviated-decode-procedure/c]
  [coerce-string
   (-> (or/c string? bytes?) string?)]
  [coerce-bytes
   (-> (or/c string? bytes?) bytes?)]
  [decode
   (-> denxi-encoding/c
       (or/c bytes? string?)
       (or/c bytes? string?))]
  [denxi-encodings
   (non-empty-listof symbol?)]
  [denxi-encoding/c
   flat-contract?]
  [encode
   (-> denxi-encoding/c
       (or/c bytes? string?)
       (or/c bytes? string?))]
  [hex abbreviated-decode-procedure/c]
  [mebibytes->bytes
   (-> real? (or/c +inf.0 exact-nonnegative-integer?))]))


(define abbreviated-decode-procedure/c
  (-> (or/c non-empty-string? bytes?) bytes?))


(define (coerce-string v)
  (if (string? v)
      v
      (bytes->string/utf-8 v)))


(define (coerce-bytes v)
  (if (bytes? v)
      v
      (string->bytes/utf-8 v)))


(define denxi-encodings
  '(base64 base32 hex colon-separated-hex))


(define denxi-encoding/c
  (apply or/c denxi-encodings))


(define (encode encoding variant)
  (define bstr (coerce-bytes variant))
  (define output
    (case encoding
      [(hex)
       (bytes->hex-string bstr)]
      [(colon-separated-hex)
       (define hexed (bytes->hex-string bstr))
       (string-join
        (for/list ([i (in-range 0 (sub1 (string-length hexed)) 2)])
          (string (string-ref hexed i) (string-ref hexed (add1 i))))
        ":")]
      [(base32) (base32-encode bstr)]
      [(base64) (base64-encode bstr #"")]))
  (if (bytes? variant)
      (coerce-bytes output)
      (coerce-string output)))


(define (decode encoding encoded)
  (case encoding
    [(hex)
     (hex-string->bytes (coerce-string encoded))]
    [(colon-separated-hex)
     (unless (regexp-match? #px"^([0-9A-Fa-f]{2}:)*[0-9A-Fa-f]{2}$" encoded)
       (raise-user-error 'decode "~v is not a valid colon-separated hex string." encoded))
     (decode 'hex (string-replace (coerce-string encoded) ":" ""))]
    [(base32)
     (base32-decode (coerce-bytes encoded))]
    [(base64)
     (base64-decode (coerce-bytes encoded))]))


(define (base32 v)
  (decode 'base32 v))


(define (base64 v)
  (decode 'base64 v))


(define (hex variant)
  (decode (if (string-contains? (coerce-string variant) ":")
              'colon-separated-hex
              'hex)
          variant))


(define (mebibytes->bytes mib)
  (if (equal? mib +inf.0)
      mib
      (inexact->exact (ceiling (* mib 1024 1024)))))


(module+ test
  (require "test.rkt")

  (test transcode
        (for ([encoding (in-list denxi-encodings)])
          (define bstr (encode encoding #"abc"))
          (assert (equal? (decode encoding bstr) #"abc"))
          (assert (equal? (decode encoding (bytes->string/utf-8 bstr)) #"abc"))))

  (test mib2b
        (assert (equal? (mebibytes->bytes +inf.0) +inf.0))
        (assert (eq? (mebibytes->bytes 0) 0))
        (assert (eqv? (mebibytes->bytes 1) 1048576))
        (assert (equal? (mebibytes->bytes (/ 1 2)) (/ 1048576 2)))))
