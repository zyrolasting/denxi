#lang racket/base

; Checks for editions/revisions

(require "contract.rkt")

(provide (contract-out [revision-number? predicate/c]
                       [revision-number-string? predicate/c]
                       [make-revision-interval
                        (-> revision-number?
                            revision-number?
                            #:lo-exclusive boolean?
                            #:hi-exclusive boolean?
                            (values revision-number? revision-number?))]))

(define (revision-number-string? v)
  (regexp-match? (pregexp "^\\d+$") v))

(define revision-number?
  (procedure-rename exact-nonnegative-integer?
                    'revision-number?))

(define (make-revision-interval lo hi #:lo-exclusive lo-exclusive? #:hi-exclusive hi-exclusive?)
  (values (if lo-exclusive? (add1 lo) lo)
          (if hi-exclusive? (sub1 hi) hi)))

(module+ test
  (require rackunit)

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
    (? 0 0 #f #f 0 0)
    (? 0 0 #t #f 1 0)
    (? 0 0 #f #t 0 -1)
    (? 0 0 #t #t 1 -1)))
