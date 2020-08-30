#lang racket/base

; Define $message, the root of a prefab structure type tree. Prefab
; structures are flexible enough to accomodate all of the following
; use cases at once:
;
;  - Visibility into data when something breaks
;  - Transmit data over place channels or ports
;  - Return values for monadic operations
;  - Compose data in custom shapes

(require "contract.rkt")
(provide (struct-out $message)
         define-message
         define+provide-message
         (contract-out
          [omit-message
           (-> any/c monad?)]
          [attach-message
           (-> any/c $message? monad?)]))

(require "monad.rkt")

(struct $message () #:prefab)

(define-syntax define-message
  (syntax-rules ()
    [(_ id super-id (fields ...))
     (struct id super-id (fields ...) #:prefab)]
    [(_ id (fields ...))
     (define-message id $message (fields ...))]))

(define-syntax-rule (define+provide-message id rem ...)
  (begin (provide (struct-out id))
         (define-message id rem ...)))

(define (omit-message v)
  (State (return v)))

(define (attach-message v m)
  (do messages <- sget
      (sput (cons m messages))
      (return v)))
