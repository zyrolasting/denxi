#lang racket/base

; Define prefab structures with flat contracts. Use to build
; machine-readable documents.

(provide (struct-out $message)
         define-message)

(require racket/contract
         (for-syntax racket/base
                     racket/contract
                     racket/syntax))

(struct $message () #:prefab)
(define-syntax $message-fields/c null)


(define-syntax (define-message stx)
  (syntax-case stx ()
    [(_ id super-id ([field contract] ...))
     (andmap identifier? (syntax-e #'(id super-id field ...)))
     (with-syntax* ([id/c (format-id #'id "~a/c" #'id)]
                    [id-fields/c (format-id #'id "~a-fields/c" #'id)]
                    [(super-id-fields/c ...)
                     (syntax-local-value
                      (format-id #'super-id "~a-fields/c" #'super-id))])
       #'(begin
           (struct id super-id (field ...) #:prefab)
           (define id/c
             (invariant-assertion
              flat-contract? (struct/c id super-id-fields/c ... contract ...)))
           (define-syntax id-fields/c
             #'(contract ...))))]
    [(_ id (f+c ...))
     #'(define-message id $message (f+c ...))]))


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
        (assert (not ($subfoo/c invalid)))
        (with-handlers ([exn:fail:contract?
                         (λ (e) (assert 'define-message-blocked))])
          (define-message $bar ([a (-> any)]))
          (assert (not 'define-message-blocked)))))
