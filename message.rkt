#lang racket/base

; Define the root of a prefab structure type tree.
; Prefab structures are flexible enough to accomodate all
; of the following use cases at once:
;
;  - Visibility into data when something breaks
;  - Transmit data over place channels or ports
;  - Return values for monadic operations
;  - Compose data in custom shapes


(provide (struct-out $message)
         define-message
         define+provide-message)

; Make prefab for use on place channels.
(struct $message () #:prefab)

(define-syntax define-message
  (syntax-rules ()
    [(_ id super-id (fields ...))
     (struct id super-id (fields ...) #:prefab)]
    [(_ id (fields ...))
     (define+provide-message id $message (fields ...))]))

(define-syntax-rule (define+provide-message id rem ...)
  (begin (provide id)
         (define-message id rem ...)))

(module+ test
  (require rackunit)

  (define-message $derived (a b c))
  (define-message $ext $derived (d))

  (test-pred "Extend $message by default" $message? ($derived 1 2 3))
  (test-pred "Extend other message if desired" $derived? ($ext 1 2 3 4)))
