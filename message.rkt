#lang racket/base

; Define prefab structures as structured data for use in machine
; readable documents.

(provide (struct-out $message)
         (struct-out $show-datum)
         (struct-out $show-string)
         coerce-$message
         define-message)

(require racket/match
         racket/pretty
         "format.rkt")


(define-syntax define-message
  (syntax-rules ()
    [(_ id super-id (fields ...))
     (struct id super-id (fields ...) #:prefab)]
    [(_ id (fields ...))
     (define-message id $message (fields ...))]))


(struct $message () #:prefab)
(define-message $show-datum  (value))
(define-message $show-string (message))


(define default-message-formatter
  (match-lambda [($show-string v) v]
                [($show-datum v) (pretty-format #:mode 'write v)]
                [v (format-value v)]))


(define (coerce-$message v)
  (if ($message? v)
      v
      ($show-string (format-value v))))


(module+ test
  (require "test.rkt")

  (define-message $foo (a b c))
  (define foo-inst ($foo 1 2 3))

  (test message
        (assert ($foo? foo-inst))
        (assert (equal? ($foo-a foo-inst) 1))
        (assert (equal? ($foo-b foo-inst) 2))
        (assert (equal? ($foo-c foo-inst) 3))))
