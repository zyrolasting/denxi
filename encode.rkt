#lang racket/base

(require racket/function
         racket/match
         "contract.rkt")

(define zcpkg-encodings
  '(base64 base32 b32 b64))

(define zcpkg-encoding/c
  (apply or/c zcpkg-encodings))

(provide (contract-out
          [zcpkg-encodings
           (non-empty-listof symbol?)]
          [zcpkg-encoding/c
           flat-contract?]
          [encode-bytes
           (-> zcpkg-encoding/c bytes? bytes?)]
          [decode-bytes
           (-> zcpkg-encoding/c bytes? bytes?)]))

(require net/base64
         base32)

(define (interpret-byte-string-expression variant)
  (match variant
    [(list (? zcpkg-encoding/c encoding)
           (? (disjoin string? bytes?) str))
     (decode-bytes encoding str)]

    [(? bytes? variant)
     variant]

    [_ (error 'interpret-byte-string-expression
              "Not a zcpkg byte expression: ~s"
              variant)]))

(define (encode-bytes encoding bstr)
  (case encoding
    [(base32 b32) (base32-encode-bytes bstr)]
    [(base64 b64) (base64-encode bstr #"")]))

(define (decode-bytes encoding encoded)
  (case encoding
    [(base32 b32)
     (base32-decode-bytes
      (if (string? encoded)
          encoded
          (bytes->string/utf-8 encoded)))]
    [(base64 b64)
     (base64-decode (if (string? encoded)
                        (string->bytes/utf-8 encoded)
                        encoded))]))

(module+ test
  (require rackunit)
  (for ([encoding (in-list zcpkg-encodings)])
    (test-equal? (format "Encode and decode a message using ~a" encoding)
                 (decode-bytes encoding (encode-bytes encoding #"abc"))
                 #"abc")
    (test-equal? (format "Interpret ~a in byte expressions" encoding)
                 (interpret-byte-string-expression `(,encoding ,(encode-bytes encoding #"abc")))
                 #"abc")))
