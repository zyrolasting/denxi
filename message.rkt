#lang racket/base

; Define an async message type and any related operations.

(provide (all-defined-out))

(require racket/string
         (for-syntax racket/base))

; Make prefab for use on place channels.
(struct $message () #:prefab)

(define-syntax-rule (define-message id (fields ...))
  (struct id $message (fields ...) #:prefab))

(define (destructure-message message)
  (let ([message (vector->list (struct->vector message))])
    (values (car message)
            (cdr message))))


(define (message-id->method-id message-id)
  (string->symbol
   (string-replace
    (symbol->string message-id)
    "struct:" "handle-")))

(module+ test
  (require rackunit)

  (define-message $derived (a b c))

  (test-case "Destructure a $message"
    (define-values (key args) (destructure-message ($derived 1 2 3)))
    (check-eq? key 'struct:$derived)
    (check-equal? args '(1 2 3))))
