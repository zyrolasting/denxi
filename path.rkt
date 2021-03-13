#lang racket/base

; Extend racket/path

(provide (all-defined-out)
         (all-from-out racket/path))

(require racket/path
         racket/set)

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

  (test-case "Detect path prefixes"
    (define path=>prefix
      '(("/a/b/c" . "/a/b")
        ("/a/b/c" . "/a/b/c/../..")
        ("." . "..")))
    (for ([pair (in-list path=>prefix)])
      (check-true (path-prefix? (car pair) (cdr pair)))
      (check-false (path-prefix? (cdr pair) (car pair))))))
