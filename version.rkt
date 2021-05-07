#lang racket/base

; Checks for editions/revisions

(require racket/format
         "contract.rkt")

(provide (contract-out [revision-number? predicate/c]
                       [revision-number-string? predicate/c]
                       [revision-number-variant? predicate/c]
                       [coerce-revision-number
                        (-> revision-number-variant? revision-number?)]
                       [coerce-revision-number-string
                        (-> revision-number-variant? revision-number-string?)]
                       [make-minimum-revision-number
                        (-> revision-number? #:exclusive? any/c revision-number?)]
                       [make-maximum-revision-number
                        (-> revision-number? #:exclusive? any/c revision-number?)]
                       [find-latest-available-revision-number
                        (-> (-> revision-number? any/c)
                            revision-number?
                            revision-number?
                            (or/c #f revision-number?))]
                       [find-oldest-available-revision-number
                        (-> (-> revision-number? any/c)
                            revision-number?
                            revision-number?
                            (or/c #f revision-number?))]))



(define (revision-number-string? v)
  (and (string? v)
       (regexp-match? (pregexp "^\\d+$") v)))

(define revision-number?
  (procedure-rename exact-nonnegative-integer?
                    'revision-number?))

(define revision-number-variant?
  (or/c revision-number?
        revision-number-string?))

(define (coerce-revision-number-string v)
  (~a v))

(define (coerce-revision-number v)
  (if (revision-number? v)
      v
      (string->number v)))

(define (make-minimum-revision-number lo-boundary #:exclusive? exclusive?)
  (if exclusive?
      (add1 lo-boundary)
      lo-boundary))

(define (make-maximum-revision-number hi-boundary #:exclusive? exclusive?)
  (if exclusive?
      (max 0 (sub1 hi-boundary))
      hi-boundary))


(define (find-latest-available-revision-number available? lo hi)
  (cond [(< hi lo) #f]
        [(available? hi) hi]
        [else (find-latest-available-revision-number
               available? lo (sub1 hi))]))

(define (find-oldest-available-revision-number available? lo hi)
  (cond [(> lo hi) #f]
        [(available? lo) lo]
        [else (find-oldest-available-revision-number
               available? (add1 lo) hi)]))


(module+ test
  (require racket/sequence rackunit)

  (test-case "Detect revision number variants"
    (define (checker c)
      (λ (v)
        (c (revision-number-variant? v))
        (c (revision-number-variant? (coerce-revision-number-string v)))))

    (sequence-for-each (checker check-true) '(100 0 9999999))
    (sequence-for-each (checker check-false) '(-1 324.9 +inf.0)))

  (test-case "Coerce revision numbers"
    (check-equal? (coerce-revision-number "100") 100)
    (check-equal? (coerce-revision-number 100) 100))

  (test-case "Coerce revision number strings"
    (check-equal? (coerce-revision-number-string "100") "100")
    (check-equal? (coerce-revision-number-string 100) "100"))

  (test-true "Accept (large) positive integers"
             (revision-number-string? "87897679687236872363278692984"))

  (test-true "Accept \"0\""
             (revision-number-string? "0"))

  (test-false "Reject negative numbers"
              (revision-number-string? "-1"))

  (test-false "Reject floating-point numbers"
              (revision-number-string? "1.224"))

  (test-false "Reject expressions"
              (revision-number-string? "1/2"))

  (test-false "Reject alphanumeric"
              (revision-number-string? "70O"))

  (test-equal? "Create minimum revision number using inclusive boundary"
               (make-minimum-revision-number 73 #:exclusive? #f)
               73)

  (test-equal? "Create minimum revision number using exclusive boundary"
               (make-minimum-revision-number 73 #:exclusive? #t)
               74)

  (test-equal? "Create maximum revision number using inclusive boundary"
               (make-maximum-revision-number 73 #:exclusive? #f)
               73)

  (test-equal? "Create maximum revision number using exclusive boundary"
               (make-maximum-revision-number 73 #:exclusive? #t)
               72)

  (test-equal? "Create maximum revision number, preventing a negative result"
               (make-maximum-revision-number 0 #:exclusive? #t)
               0)

  (test-case "Search revision numbers by availability"
    (define available '(0 8 31 11 99))
    (define (available? v) (member v available))

    (check-equal? (find-latest-available-revision-number available? 0 10) 8)
    (check-equal? (find-latest-available-revision-number available? 0 8) 8)
    (check-equal? (find-latest-available-revision-number available? 0 7) 0)
    (check-equal? (find-latest-available-revision-number available? 0 100) 99)

    (check-equal? (find-oldest-available-revision-number available? 0 10) 0)
    (check-equal? (find-oldest-available-revision-number available? 1 100) 8)
    (check-equal? (find-oldest-available-revision-number available? 9 100) 11)))
