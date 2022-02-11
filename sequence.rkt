#lang racket/base

(require racket/contract)

(provide (all-from-out racket/sequence)
         (contract-out
          [in-cartesian-product
           (-> (sequence/c sequence?)
               (sequence/c vector?))]
          [in-cartesian-map
           (-> procedure?
               (sequence/c sequence?)
               sequence?)]
          [odometer
           (-> (sequence/c sequence?)
               (values exact-nonnegative-integer?
                       (-> exact-nonnegative-integer? vector?)))]))


(require racket/sequence)

;--------------------------------------------------------------------------------
; Lazy n-ary cartesian product
; Use sequences as pascaline-style gears. Present as odometer abstraction.

(require racket/function
         racket/promise)

(module+ test
  (require racket/list
           racket/pretty
           "test.rkt")

  ; Compare output of this library to Racket's built-in
  ; cartesian-product. The test passes if they agree on
  ; the counting pattern.

  (define (my-cartesian-product . v)
    (sequence->list
     (sequence-map vector->list
                   (in-cartesian-product v))))


  (define (agree? . v)
    (define expected
      (apply cartesian-product
             (map sequence->list v)))
    (define actual
      (apply my-cartesian-product v))
    (assert (equal? actual expected)))


  (test agree-with-racket
        (define gear
          (in-range 3))
        (apply agree?
               (build-list (random 3 5)
                           (λ (n)
                             (in-range (random 3 5))))))

  (test between-sets
    (define (in-char-range start end)
      (sequence-map integer->char (in-range start end)))

    (define (in-ab-two-permutations)
      (define a-or-b (in-char-range 97 99))
      (in-cartesian-map string (in-list (list a-or-b a-or-b))))

    (assert (equal? (sequence->list (in-ab-two-permutations))
                    '("aa" "ab" "ba" "bb")))))


(define (in-cartesian-map f gears)
  (sequence-map (lambda (vec) (call-with-values (lambda () (vector->values vec)) f))
                (in-cartesian-product gears)))


(define (in-cartesian-product gears)
  (define-values (upper-bound f) (odometer gears))
  (sequence-map f (in-range (add1 upper-bound))))


; Generalized odometer: Rotate the first gear of a simple transmission
; in discrete increments. Each side of each gear has a label, such that a
; transmission state presents a reading.
(define (odometer user-specification)
  (define gears
    (apply vector (sequence->list user-specification)))

  (define gear-count
    (vector-length gears))

  (define unit-position
    (sub1 gear-count))

  (define orientations
    (for/vector ([labels gears])
      (sequence-length labels)))

  (define turns-required
    (make-vector gear-count))
  (let loop ([position unit-position])
    (when (>= position 0)
      (vector-set! turns-required
                   position
                   (if (= position unit-position)
                       1
                       (for/product ([i (in-range (add1 position) gear-count)])
                         (vector-ref orientations i))))
      (loop (sub1 position))))


  (define max-user-ordinal
    (sub1 (sequence-fold * 1 (in-vector orientations))))

  (define (odometer-state user-ordinal)
    (define ordinal (max 0 (min user-ordinal max-user-ordinal)))
    (for/vector ([(gear position) (in-indexed gears)])
      (define turns-to-revolve (vector-ref turns-required position))
      (define revolutions (quotient ordinal turns-to-revolve))
      (define orientation (modulo revolutions (vector-ref orientations position)))
      (sequence-ref gear orientation)))

  (values max-user-ordinal
          odometer-state))
