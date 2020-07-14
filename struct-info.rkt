#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base
                     racket/match
                     racket/struct-info))

; Expand to procedures that integrate a struct definition with lookup procedures.
(define-syntax (accessor-names stx)
  (syntax-case stx ()
    [(_ struct-id)
     (match-let ([(list _ _ _ accessors _ _)
                  (extract-struct-info (syntax-local-value #'struct-id))])
       #`(list . #,(map (compose symbol->string syntax->datum)
                        accessors)))]))
