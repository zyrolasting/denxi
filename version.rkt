#lang racket/base

; Checks for editions/revisions

(require "contract.rkt"
         racket/format)

(provide (contract-out [revision-number? predicate/c]
                       [revision-number-string? predicate/c]
                       [revision-number-variant? predicate/c]
                       [coerce-revision-number
                        (-> revision-number-variant? revision-number?)]
                       [coerce-revision-number-string
                        (-> revision-number-variant? revision-number-string?)]
                       [make-revision-interval
                        (-> (or/c #f revision-number?)
                            (or/c #f revision-number?)
                            #:lo-exclusive boolean?
                            #:hi-exclusive boolean?
                            (values revision-number? revision-number?))]))

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

(define (make-revision-interval lo hi #:lo-exclusive lo-exclusive? #:hi-exclusive hi-exclusive?)
  (values (if (and lo lo-exclusive?) (add1 lo) lo)
          (if (and hi hi-exclusive?) (sub1 hi) hi)))

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

  (test-case "Create revision intervals"
    (define (? lo hi le he el eh)
      (call-with-values (λ () (make-revision-interval lo hi #:lo-exclusive le #:hi-exclusive he))
                        (λ (al ah)
                          (check-equal? al el)
                          (check-equal? ah eh))))
    ; Normal operation
    (? 0 0 #f #f 0 0)
    (? 0 0 #t #f 1 0)
    (? 0 0 #f #t 0 -1)
    (? 0 0 #t #t 1 -1)

    ; Confirm that missing data still appears missing
    (? #f #f #f #f #f #f)
    (? #f #f #t #f #f #f)
    (? #f #f #f #t #f #f)
    (?  1 #f #t #t 2  #f)
    (? #f  1 #f #t #f 0)))
