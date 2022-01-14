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
  (require rackunit racket/list racket/pretty)

  ; Compare output of this library to Racket's built-in
  ; cartesian-product. The test passes if they agree on
  ; the counting pattern.

  (define (agree? . v)
    (define expected
      (apply cartesian-product
             (map sequence->list v)))

    (define actual
      (sequence->list
       (sequence-map vector->list
                     (in-cartesian-product v))))

    (equal? actual expected))

  (define gear (in-range 10))

  (check-true (agree? gear gear gear))

  (test-case "Map element of cartesian product to elements of other sets"
    (define (in-char-range start end)
      (sequence-map integer->char (in-range start end)))

    (define (in-ab-two-permutations)
      (define a-or-b (in-char-range 97 99))
      (in-cartesian-map string (in-list (list a-or-b a-or-b))))

    (check-equal? (sequence->list (in-ab-two-permutations))
                  '("aa" "ab" "ba" "bb"))))


(define (in-cartesian-map f gears)
  (sequence-map (lambda (vec) (call-with-values (lambda () (vector->values vec)) f))
                (in-cartesian-product gears)))


(define (in-cartesian-product gears)
  (define-values (upper-bound f) (odometer gears))
  (sequence-map f (in-range (add1 upper-bound))))


(define (odometer gears)
  ; There are as many digit positions as gears in the odometer.  Also,
  ; knowing the cardinality (available orientations) of each gear
  ; means we can find out how many times each revolved. Both of these
  ; expressions block when given infinite sequences.
  (define available-positions
    (cardinality gears))
  (define known-cardinalities
    (cardinalities gears))

  ; Translate the number of possible readings of the abstract odometer
  ; to the highest reading a normal odometer can have. e.g. by the
  ; time the odomoter of your car reads 083218, the abstract odomoter
  ; may have reached the end.
  (define max-user-ordinal
    (sub1 (cartesian-product-cardinality known-cardinalities)))

  ; Return the number of times a gear revolved completely in position.
  ; The unit gear is trivial because it rotated as many times as there
  ; were increments in general. Other gears rotated at a speed dependent
  ;
  ; Not tail recursive; linear traversal
  (define (revolutions target-digit-position cartesian-product-ordinal)
    (if (= target-digit-position (sub1 available-positions))
        cartesian-product-ordinal
        (let* ([divisor (revolutions (add1 target-digit-position) cartesian-product-ordinal)]
               [dividend (ref known-cardinalities target-digit-position)])
          (inexact->exact (quotient divisor dividend)))))


  ; Cache repeated work because the algorithm always computes every
  ; gear in response to a specific natural number.
  (define promises (make-hash))
  (define (make-promise x y)
    (delay (revolutions x y)))
  (define (revolutions/cached position cartesian-product-ordinal)
    (define by-ordinal (hash-ref! promises cartesian-product-ordinal))
    (define fail-thunk (thunk (make-promise position cartesian-product-ordinal)))
    (force (hash-ref! by-ordinal fail-thunk)))

  ; Glue it all to the natural numbers.
  (define (find-odometer-reading user-ordinal)
    (define cartesian-product-ordinal
      (max 0 (min user-ordinal max-user-ordinal)))
    (for/vector ([position (in-range available-positions)])
      (define gear (ref gears position))
      (define number-of-revolutions (revolutions position cartesian-product-ordinal))
      (define number-of-orientations (ref known-cardinalities position))
      (define relevant-orientation (modulo number-of-revolutions number-of-orientations))
      (ref gear relevant-orientation)))

  ; Give user the upper bound for context.
  (values max-user-ordinal find-odometer-reading))


;--------------------------------------------------------------------------------
; Set notation for sequences

(define cardinality sequence-length)
(define ref sequence-ref)

(define (cardinalities s)
  (sequence-map cardinality s))

(define (cartesian-product-cardinality cardinalities)
  (sequence-fold * 1 cardinalities))
