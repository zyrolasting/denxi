#lang racket/base

; Define prefab structures with flat contracts. Use to build
; machine-readable documents.

(provide (struct-out $message)
         (struct-out $show-datum)
         (struct-out $show-string)
         default-message-formatter
         define-message)

(require racket/contract
         (for-syntax racket/base
                     racket/syntax))


(define-syntax (define-message stx)
  (syntax-case stx ()
    [(_ id super-id ([field contract] ...))
     (andmap identifier? (syntax-e #'(id super-id field ...)))
     (with-syntax ([id/c (format-id #'id "~a/c" #'id)])
       #'(begin (struct id super-id (field ...) #:prefab)
                (define id/c (invariant-assertion flat-contract? (struct/c id contract ...)))))]
    [(_ id (f+c ...))
     #'(define-message id $message (f+c ...))]))


(struct $message () #:prefab)
(define-message $show-datum  ([value any/c]))
(define-message $show-string ([value string?]))


(define (default-message-formatter v)
  (cond [($show-string/c v) ($show-string-value v)]
        [($show-datum/c v) (format "~s" ($show-datum-value v))]
        [else (format "~s" v)]))


(module+ test
  (require "test.rkt")

  (define-message $foo ([a real?] [b real?] [c real?]))

  (test message-structure
        (define foo-inst ($foo 1 2 3))
        (assert ($foo? foo-inst))
        (assert (equal? ($foo-a foo-inst) 1))
        (assert (equal? ($foo-b foo-inst) 2))
        (assert (equal? ($foo-c foo-inst) 3)))

  (test message-contracts
        (assert ($foo/c ($foo 1 2 3)))
        (assert (not ($foo/c ($foo "1" 2 3))))
        (with-handlers ([exn:fail:contract?
                         (λ (e) (assert 'define-message-blocked))])
          (define-message $bar ([a (-> any)]))
          (assert (not 'define-message-blocked)))))
