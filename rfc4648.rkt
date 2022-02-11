#lang racket/base

; Implementation of RFC 4648. Currently only includes a Base32 codec.

; Because these are a pain to type otherwise.
(define <<  arithmetic-shift)
(define  &  bitwise-and)
(define ||  bitwise-ior)


;-------------------------------------------------------------------------------
; Base 32 Codec
;
; The existing implementations of base32 for Racket are either under a
; GPL license or unlicensed. They use a method that manually accounts
; for bits, followed by some math to compute necessary padding.  To
; mitigate the risk of copyright claims I opted to write unrolled
; loops based on a literal interpretation of the RFC, and allow for
; configurable alphabets.

(provide base32-encode
         base32-decode
         base32-rfc-alphabet
         base32-crockford-alphabet)

(define base32-rfc-alphabet         #"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")
(define base32-crockford-alphabet   #"0123456789abcdefghjkmnpqrstvwxyz")

(define (process-bytes bstr continue . args)
  (let ([out (open-output-bytes)])
    (apply continue (open-input-bytes bstr) out args)
    (get-output-bytes out #t)))


(define (base32-encode bstr [alphabet base32-crockford-alphabet] [padding-byte (char->integer #\=)])
  (process-bytes bstr base32-encode/ports alphabet padding-byte))

(define (base32-decode bstr [alphabet base32-crockford-alphabet] [padding-byte (char->integer #\=)])
  (process-bytes bstr base32-decode/ports alphabet padding-byte))


; RFC4648 sec. 6 reasons about 8- 16- 24-, 32-, and 40-bit
; "quantums". This procedure assumes no incomplete bytes will be read,
; and applies a continuation that follows the itemized casework
; near the end of the section.
;
(define (read-quantum in continue byte-buffer bytes-read)
  (if (= bytes-read 5)
      (continue byte-buffer 5 (λ () (read-quantum in continue 0 0)))
      (let ([item-read (read-byte in)])
        (if (eof-object? item-read)
            (continue byte-buffer bytes-read void)
            (read-quantum in
                          continue
                          (|| (<< byte-buffer 8) item-read)
                          (add1 bytes-read))))))


(define (base32-encode/ports in out alphabet padding-byte)
  (read-quantum in
                (λ (byte-buffer bytes-read continue)
                  (define (<- shift [mask 31])
                    (write-byte (bytes-ref alphabet (& (<< byte-buffer shift) mask)) out))
                  (define (pad!)
                    (write-byte padding-byte out))
                  (case bytes-read
                    [(1) (<- -3)  (<- 2)   (pad!)   (pad!)    (pad!)    (pad!)   (pad!)   (pad!)]
                    [(2) (<- -11) (<- -6)  (<- -1)  (<- 4)    (pad!)    (pad!)   (pad!)   (pad!)]
                    [(3) (<- -19) (<- -14) (<- -9)  (<- -4)   (<- 1)    (pad!)   (pad!)   (pad!)]
                    [(4) (<- -27) (<- -22) (<- -17) (<- -12)  (<- -7)   (<- -2)  (<- 3)   (pad!)]
                    [(5) (<- -35) (<- -30) (<- -25) (<- -20)  (<- -15)  (<- -10) (<- -5)  (<- 0)])
                  (continue))
                0
                0))


(define (base32-decode/ports in out alphabet padding-byte)
  (base32-decode/ports/iter in out alphabet padding-byte
                            0 0
                            (for/hash ([i (in-range (bytes-length alphabet))])
                              (values (bytes-ref alphabet i) i))))


(define (base32-decode/ports/iter in out alphabet padding-byte byte-buffer bits-left byte->index)
  (if (>= bits-left 8)
      (let ([bits-left* (- bits-left 8)])
        (write-byte (<< byte-buffer (- bits-left*)) out)
        (base32-decode/ports/iter in
                                  out
                                  alphabet
                                  padding-byte
                                  (& byte-buffer (sub1 (<< 1 bits-left*)))
                                  bits-left*
                                  byte->index))
      (let ([next-byte (read-byte in)])
        (cond [(eof-object? next-byte) (void)]
              [(equal? next-byte padding-byte) (void)]
              [(hash-ref byte->index next-byte -1)
               => (λ (index)
                    (if (= index -1)
                        (base32-decode/ports/iter in
                                                  out
                                                  alphabet
                                                  padding-byte
                                                  byte-buffer
                                                  bits-left
                                                  byte->index)
                        (base32-decode/ports/iter in
                                                  out
                                                  alphabet
                                                  padding-byte
                                                  (|| (<< byte-buffer 5) index)
                                                  (+ bits-left 5)
                                                  byte->index)))]))))


(define (~b v)
  (local-require racket/format)
  (~r #:min-width 8 #:pad-string "0" #:base 2 v))


(module+ test
  (require "test.rkt")

  ; https://tools.ietf.org/html/rfc4648
  (define function
    '((#"" . #"")
      (#"f" . #"cr======")
      (#"fo" . #"csqg====")
      (#"foo" . #"csqpy===")
      (#"foob" . #"csqpyrg=")
      (#"fooba" . #"csqpyrk1")
      (#"foobar" . #"csqpyrk1e8======")))

  (test rfc4648
        (for ([pairing (in-list function)])
          (assert (equal? (base32-encode (car pairing)) (cdr pairing)))
          (assert (equal? (base32-decode (cdr pairing)) (car pairing))))))
