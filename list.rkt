#lang racket/base

(provide (all-from-out racket/list)
         (all-defined-out))

(require racket/list)

(define (assoc+ v l [def #f])
  (or (let ([el (findf (λ (subject) (equal? v (car subject))) l)])
        (and el (cadr el)))
      def))

(module+ test
  (require rackunit)

  (test-equal? "Find element in association list, where values appear in the cadr of an element"
               (assoc+ 'cool '((cool 1)))
               1)

  (test-equal? "Fall back to default when needed"
               (assoc+ 'blah '((cool 1)))
               #f)

  (test-equal? "Fall back to user-specified default when needed"
               (assoc+ 'blah '((cool 1)) 2)
               2))
