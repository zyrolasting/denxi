#lang racket/base

; Extend racket/path

(provide (all-defined-out)
         (all-from-out racket/path))

(require racket/path)

(define (path-prefix? to-check prefix-pathy)
  (define maybe-prefixed (explode-path (simplify-path (path->complete-path to-check))))
  (define pref (explode-path (simplify-path (path->complete-path prefix-pathy))))

  (and (<= (length pref)
           (length maybe-prefixed))
       (for/and ([(el index) (in-indexed pref)])
         (equal? (list-ref maybe-prefixed index)
                 el))))


(module+ test
  (require "test.rkt")
  (test path-prefix?
    (define paths '("/a/b/c" "."))
    (for ([p (in-list paths)])
      (assert (path-prefix? p (build-path p 'up)))
      (assert (not (path-prefix? (build-path p 'up) p))))))
