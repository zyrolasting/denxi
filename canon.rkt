#lang racket/base

(require racket/contract
         racket/function)       

(provide canon
         (contract-out
          [logomorphism
           (-> string? any/c string?)]))


(define-syntax-rule (canon [prescription experience] ...)
  (λ (utterance)
    (or (logomorphism prescription experience utterance)
        ...)))


(define (prescribe?/regexp prescription experience utterance)
  (and (regexp-like? experience)
       (regexp-match? experience utterance)
       prescription))


(define (prescribe?/exact prescription experience utterance)
  (and (string? experience)
       (string=? experience utterance)
       prescription))


(define (prescribe?/one-of prescription experience utterance)
  (and (list? experience)
       (member utterance experience)
       prescription))


(define (prescribe?/procedure prescription experience utterance)
  (and (procedure? experience)
       (experience utterance)
       prescription))


(define logomorphism
  (disjoin prescribe?/regexp
           prescribe?/exact
           prescribe?/one-of
           prescribe?/procedure))


(define regexp-like?
  (disjoin regexp?
           pregexp?
           byte-pregexp?
           byte-regexp?))


(module+ test
  (require rackunit)

  (define c
    (canon ["fish" "salmon"]
           ["colors" '("blue" "green")]
           ["question" #px"\\?$"]
           ["capitalized" (λ (s) (equal? (string-upcase s) s))]))

  (check-equal? (c "salmon") "fish")
  (check-equal? (c "blue") (c "green"))
  (check-equal? (c "blue") "colors")
  (check-equal? (c "what do you think?") "question")
  (check-equal? (c "ABC") "capitalized")
  (check-false  (c "something else")))
