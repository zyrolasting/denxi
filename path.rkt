#lang racket/base

; Extend racket/path

(provide (all-defined-out)
         (all-from-out racket/path))

(require racket/path)

(define (../ path)
  (simplify-path (build-path path 'up)))

(define (path-prefix? to-check prefix-pathy)
  (define maybe-prefixed (explode-path (simplify-path (path->complete-path to-check))))
  (define pref (explode-path (simplify-path (path->complete-path prefix-pathy))))

  (and (<= (length pref)
           (length maybe-prefixed))
       (for/and ([(el index) (in-indexed pref)])
         (equal? (list-ref maybe-prefixed index)
                 el))))

(module+ test
  (require racket/file
           rackunit)

  (test-case "Do not bypass root directory in upward traversals"
    (for ([root (filesystem-root-list)])
      (check-equal? root (../ root))))

  (test-case "Detect path prefixes"
    (define paths '("/a/b/c" "."))
    (for ([p (in-list paths)])
      (check-true (path-prefix? p (../ p)))
      (check-false (path-prefix? (../ p) p)))))
