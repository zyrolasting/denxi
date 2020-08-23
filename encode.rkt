#lang racket/base

; Define supported decoding and encoding algorithms and a simple
; interface for them all.

(require racket/contract
         net/base64
         base32)

(provide (contract-out
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
               (or/c bytes? string?))]))

(define xiden-encodings
  '(base64 base32))

(define xiden-encoding/c
  (apply or/c xiden-encodings))

(define (encoded-file-name variant)
  (define encoded (encode 'base32 variant))
  (define as-string
    (if (bytes? encoded)
        (bytes->string/utf-8 encoded)
        encoded))

  (substring as-string 0
             (min (string-length as-string) 32)))


(define (encode encoding variant)
  (define bstr
    (if (bytes? variant)
        variant
        (string->bytes/utf-8 variant)))

  (define output
    (case encoding
      [(base32) (base32-encode-bytes bstr)]
      [(base64) (base64-encode bstr #"")]))

  (if (bytes? variant)
      (if (string? output)
          (string->bytes/utf-8 output)
          output)
      (if (bytes? output)
          (bytes->string/utf-8 output)
          output)))

(define (decode encoding encoded)
  (case encoding
    [(base32)
     (base32-decode-bytes
      (if (string? encoded)
          encoded
          (bytes->string/utf-8 encoded)))]
    [(base64)
     (base64-decode (if (string? encoded)
                        (string->bytes/utf-8 encoded)
                        encoded))]))

(module+ test
  (require rackunit)
  (for ([encoding (in-list xiden-encodings)])
    (test-case (format "Encode and decode a message using ~a" encoding)
      (define bstr (encode encoding #"abc"))
      (check-equal? (decode encoding bstr) #"abc")
      (check-equal? (decode encoding (bytes->string/utf-8 bstr)) #"abc"))))
