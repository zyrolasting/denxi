#lang racket/base

(require "contract.rkt")

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

(define (encode-bytes encoding bstr)
  (case encoding
    [(base32 b32) (base32-encode-bytes bstr)]
    [(base64 b64) (base64-encode bstr #"")]))

(define (decode-bytes encoding encoded)
  (case encoding
    [(base32 b32) (base32-decode-bytes encoded)]
    [(base64 b64) (base64-decode encoded)]))

(module+ test
  (require rackunit)
  (for ([encoding (in-list zcpkg-encodings)])
    (test-equal? (format "Encode and decode a message using ~a" encoding)
                 (decode-bytes encoding (encode-bytes encoding #"abc"))
                 #"abc")))
