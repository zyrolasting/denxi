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
  #:∃ granny
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
  [give-patch (-> granny bytes? exact-nonnegative-integer? void?)]
  [granny-stitching? (-> granny boolean?)]
  [granny-quilt-ready? (-> granny predicate/c)]
  [hex abbreviated-decode-procedure/c]
  [make-granny
   (-> bytes? granny)]
  [mebibytes->bytes
   (-> real? (or/c +inf.0 exact-nonnegative-integer?))]
  [patch-bytes
   (->* (bytes?
         exact-nonnegative-integer?
         bytes?)
        (exact-positive-integer?
         #:padding-byte byte?)
        bytes?)]
  [take-quilt
   (-> granny (and/c bytes? immutable?))]))


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


;--------------------------------------------------------------------------------
; Grannies stitch quilts.

(struct granny
  (patch-size
   [patches-given #:mutable]
   [patches-added #:mutable]
   design-in
   quilts-out
   [spool #:mutable]
   [quilt #:mutable])
  #:property prop:evt 4)


(define (make-granny initial-square)
  (define design-in  (make-async-channel))
  (define quilts-out (make-async-channel))
  (define granny-out
    (granny (bytes-length initial-square)
            0
            0
            design-in
            quilts-out
            #f
            ((if (immutable? initial-square)
                 bytes-copy
                 values)
             initial-square)))

  (define (add-patches)
    (let stitch ()
      ((sync design-in))
      (set-granny-patches-added! granny-out
                                 (add1 (granny-patches-added granny-out)))
      (stitch)))

  (set-granny-spool! granny-out (thread add-patches))

  granny-out)


(define (granny-quilt-ready? g)
  (equal? (granny-patches-given g)
          (granny-patches-added g)))


(define (granny-stitching? g)
  (let ([spool (granny-spool g)])
    (and (thread? spool)
         (thread-running? spool))))


(define (take-quilt g)
  (when (granny-stitching? g)
    (let loop () (and (sync/timeout #f g) (loop)))
    (kill-thread (granny-spool g))
    (set-granny-quilt! g (unsafe-bytes->immutable-bytes! (granny-quilt g))))
  (granny-quilt g))


(define (give-patch g src dest-start)
  (async-channel-put
   (granny-design-in g)
   (λ ()
     (define new-quilt
       (patch-bytes (granny-quilt g)
                    dest-start
                    src
                    (granny-patch-size g)))
     (set-granny-quilt! g new-quilt)
     (set-granny-patches-given! g (add1 (granny-patches-given g)))
     (async-channel-put (granny-quilts-out g) new-quilt)))
  g)


(define (patch-bytes proposed-dest dest-start src [buffer-length 4096]
                     #:padding-byte [padding-byte 0])
  (define available-length
    (bytes-length proposed-dest))

  (define patch-length
    (bytes-length src))

  (define required-length/bytes
    (max available-length
         (+ dest-start patch-length)))

  (define buffer-units
    (ceiling (/ required-length/bytes buffer-length)))

  (define required-length/buffer-aligned
    (inexact->exact (* buffer-length buffer-units)))

  (define dest
    (if (< available-length required-length/buffer-aligned)
        (bytes-append proposed-dest
                      (make-bytes (- required-length/buffer-aligned available-length)
                                  padding-byte))
        (if (immutable? proposed-dest)
            (bytes-copy proposed-dest)
            proposed-dest)))

  (bytes-copy! dest dest-start src)

  dest)


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
        (assert (equal? (mebibytes->bytes (/ 1 2)) (/ 1048576 2))))

  (test patch-greeting
    (define sample
      (subbytes #"hello world" 0))

    (assert (equal? (patch-bytes #:padding-byte 1 sample 6 #"padding" 8)
                    #"hello padding\1\1\1"))

    (assert (eq? (patch-bytes sample 0 sample 11)
                 sample)))

  (test hypothetical-granny
    (define quilts (make-async-channel))
    (define g (make-granny #"Where's the YouTubes on my phone?"))
    (assert (granny-stitching? g))

    (define 1st (sync (give-patch g #"pictures" 12)))
    (assert (equal? 1st #"Where's the pictures on my phone?"))

    (define 2nd (sync (give-patch g #"telly" 27)))
    (assert (eq? 2nd 1st)) ; Check for no reallocation
    (assert (equal? 2nd #"Where's the pictures on my telly?"))

    (define 3rd (sync (give-patch g #" Come help gramma!" 33)))
    (assert (equal? 3rd
                    (bytes-append #"Where's the pictures on my telly?"
                                  #" Come help gramma!"
                                  (make-bytes 15))))

    (define final (take-quilt g))
    (assert (equal? final 3rd))
    (assert (immutable? final))
    (assert (granny-quilt-ready? g))
    (assert (not (granny-stitching? g)))))
