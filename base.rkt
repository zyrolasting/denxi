#lang racket/base

(define-syntax-rule (reprovide x ...)
  (begin (begin (require x)
                (provide (all-from-out x)))
         ...))

(provide reprovide)

(reprovide racket/base
           racket/contract)
