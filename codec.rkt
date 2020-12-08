#lang racket/base

; Decode and encode byte strings with prescribed algorithms.

(require racket/contract
         racket/string
         file/sha1
         net/base64
         "rfc4648.rkt")

(define abbreviated-decode-procedure/c
  (-> (or/c non-empty-string? bytes?) bytes?))

(provide (contract-out
          [abbreviated-decode-procedure/c
           chaperone-contract?]
          [coerce-string
           (-> (or/c string? bytes?) string?)]
          [coerce-bytes
           (-> (or/c string? bytes?) bytes?)]
          [xiden-encodings
           (non-empty-listof symbol?)]
          [xiden-encoding/c
           flat-contract?]
          [encoded-file-name
           (-> (or/c bytes? string?)
               string?)]
          [encode
           (-> xiden-encoding/c
               (or/c bytes? string?)
               (or/c bytes? string?))]
          [decode
           (-> xiden-encoding/c
               (or/c bytes? string?)
               (or/c bytes? string?))]
          [base32 abbreviated-decode-procedure/c]
          [base64 abbreviated-decode-procedure/c]
          [hex abbreviated-decode-procedure/c]))


(define (coerce-string v)
  (if (string? v)
      v
      (bytes->string/utf-8 v)))


(define (coerce-bytes v)
  (if (bytes? v)
      v
      (string->bytes/utf-8 v)))


(define xiden-encodings
  '(base64 base32 hex colon-separated-hex))


(define xiden-encoding/c
  (apply or/c xiden-encodings))


(define (encoded-file-name variant)
  (let ([as-string (coerce-string (encode 'base32 variant))])
    (substring (coerce-string as-string) 0
               (min (string-length as-string) 32))))


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


(module+ test
  (require rackunit)
  (for ([encoding (in-list xiden-encodings)])
    (test-case (format "Encode and decode a message using ~a" encoding)
      (define bstr (encode encoding #"abc"))
      (check-equal? (decode encoding bstr) #"abc")
      (check-equal? (decode encoding (bytes->string/utf-8 bstr)) #"abc"))))
