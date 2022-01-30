#lang racket/base

(require racket/contract
         racket/function)

(provide (all-from-out racket/function)
         async
         (contract-out
          [keep-values (unconstrained-domain-> (-> any))]))

(define-syntax-rule (async . body)
  (thread (lambda () . body)))

(define ((keep-values . v))
  (apply values v))
