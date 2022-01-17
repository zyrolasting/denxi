#lang racket/base

(require racket/contract)
(provide
 (contract-out
  #:∃ granny
  [make-granny
   (-> bytes? granny)]
  [granny-stitching?
   (-> granny boolean?)]
  [granny-quilt-ready?
   (-> granny predicate/c)]
  [take-quilt
   (-> granny (and/c bytes? immutable?))]
  [give-patch
   (-> granny bytes? exact-nonnegative-integer? void?)]
  [patch-bytes
   (->* (bytes?
         exact-nonnegative-integer?
         bytes?)
        (exact-positive-integer?
         #:padding-byte byte?)
        bytes?)]))


(require racket/async-channel
         racket/unsafe/ops)

(module+ test
  (require rackunit)

  (test-case "Patch mutable byte string"
    (define sample
      (subbytes #"hello world" 0))

    (check-equal? (patch-bytes #:padding-byte 1 sample 6 #"padding" 8)
                  #"hello padding\1\1\1")

    (check-eq? (patch-bytes sample 0 sample 11)
               sample))

  (test-case "Stitch a byte quilt with a granny"
    (define quilts (make-async-channel))
    (define g (make-granny #"Where's the YouTubes on my phone?"))
    (check-pred granny-stitching? g)

    (define 1st (sync (give-patch g #"pictures" 12)))
    (check-equal? 1st #"Where's the pictures on my phone?")

    (define 2nd (sync (give-patch g #"telly" 27)))
    (check-eq? 2nd 1st) ; Check for no reallocation
    (check-equal? 2nd #"Where's the pictures on my telly?")

    (define 3rd (sync (give-patch g #" Come help gramma!" 33)))
    (check-equal? 3rd
                  (bytes-append #"Where's the pictures on my telly?"
                                #" Come help gramma!"
                                (make-bytes 15)))

    (define final (take-quilt g))
    (check-equal? final 3rd)
    (check-pred immutable? final)
    (check-pred granny-quilt-ready? g)
    (check-false (granny-stitching? g))))


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