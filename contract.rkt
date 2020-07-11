#lang racket/base

(provide (all-from-out racket/contract)
         (all-defined-out))

(require racket/contract)

(define (passes-invariant-assertion? c v)
  (with-handlers ([exn:fail:contract? (λ (e) #f)])
    (invariant-assertion c v)
    #t))
