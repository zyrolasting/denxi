#lang racket/base

; Define prefab structures with flat contracts. Use to build
; machine-readable documents.

(provide (struct-out $message)
         define-message)

(require syntax/parse/define
         racket/contract
         (for-syntax racket/base
                     racket/struct-info
                     racket/syntax))

; With thanks to @sorawee for help
(begin-for-syntax
  (define-values (imp-prop:struct-contract
                  imp-prop:struct-contract?
                  imp-prop:struct-contract-get)
    (make-impersonator-property 'imp-prop:struct-contract)))

(define-syntax-parse-rule (define-message x:id {~optional super:id} ([field:id contract] ...))
  #:with (super-contract ...)
  (cond [(attribute super)
         (imp-prop:struct-contract-get (syntax-local-value #'super))]
        [else #'()])
  #:with (current-contract ...) #'(super-contract ... contract ...)
  #:with x/c (format-id this-syntax "~a/c" #'x)
  #:with x-cons (format-id this-syntax "~a-constructor" #'x)
  (begin (struct x {~? super} (field ...) #:prefab
           #:name x-cons
           #:constructor-name x-cons)
         (define-syntax x
           (impersonate-struct
            (syntax-local-value #'x-cons)
            struct:struct-info
            imp-prop:struct-contract
            #'(current-contract ...)))
         (define x/c (struct/c x-cons (invariant-assertion flat-contract? current-contract) ...))))

(define-message $message ())


(module+ test
  (require "test.rkt")

  (define-message $foo ([a real?]))
  (define-message $subfoo $foo ([a string?]))
  (define valid ($subfoo 1 "b"))
  (define invalid ($subfoo 1 2))

  (test message-structure
        (assert ($foo? valid))
        (assert ($subfoo? valid))
        (assert (equal? ($foo-a valid) 1))
        (assert (equal? ($subfoo-a valid) "b")))

  (test message-contracts
        (assert ($foo/c valid))
        (assert ($subfoo/c valid))
        (assert ($foo/c invalid))
        (assert (not ($subfoo/c invalid)))))
