#lang racket/base

(require racket/contract)

(provide (all-from-out racket/contract)
         (contract-out
          [passes-invariant-assertion?
           (-> flat-contract? any/c boolean?)]
          [rewrite-contract-error-message
           (-> exn:fail:contract? symbol? exn:fail:contract?)]))

; TODO: Flat contracts can be used as predicates. Remove?
(define (passes-invariant-assertion? c v)
  (with-handlers ([exn:fail:contract? (λ (e) #f)])
    (invariant-assertion c v)
    #t))

(define (rewrite-contract-error-message e id)
  (struct-copy exn:fail:contract e
               [message #:parent exn
                        (regexp-replace #rx"^[^\n]+"
                                        (exn-message e)
                                        (format "Invalid value for ~a" id))]))
