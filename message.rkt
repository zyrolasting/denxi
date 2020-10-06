#lang racket/base

; Define $message, the root of a prefab structure type tree. Prefab
; structures are flexible enough to accomodate all of the following
; use cases at once:
;
;  - Visibility into data when something breaks
;  - Transmit data over place channels or ports
;  - Return values for monadic operations
;  - Compose data in custom shapes

(provide (struct-out $message)
         define-message
         define+provide-message)

(define-syntax define-message
  (syntax-rules ()
    [(_ id super-id (fields ...))
     (struct id super-id (fields ...) #:prefab)]
    [(_ id (fields ...))
     (define-message id $message (fields ...))]))

(define-syntax-rule (define+provide-message id rem ...)
  (begin (provide (struct-out id))
         (define-message id rem ...)))


(struct $message () #:prefab)
(define+provide-message $show-datum  (value))
(define+provide-message $show-string (message))

(module+ test
  (require rackunit)

  (define-message $foo (a b c))
  (define foo-inst ($foo 1 2 3))

  (check-pred $foo? foo-inst)
  (check-equal? ($foo-a foo-inst) 1)
  (check-equal? ($foo-b foo-inst) 2)
  (check-equal? ($foo-c foo-inst) 3))
