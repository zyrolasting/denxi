#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/path))

(require racket/function
         racket/list
         racket/path
         racket/set)

(define (../ path)
  (simplify-path (build-path path 'up)))

(define (strip-ups path)
  (define filtered (filter-not (curry eq? 'up) (explode-path path)))
  (and (not (null? filtered))
       (apply build-path filtered)))

(define (path-prefix? to-check prefix-pathy)
  (define maybe-prefixed (explode-path (simplify-path (path->complete-path to-check))))
  (define pref (explode-path (simplify-path (path->complete-path prefix-pathy))))

  (and (<= (length pref)
           (length maybe-prefixed))
       (for/and ([(el index) (in-indexed pref)])
         (equal? (list-ref maybe-prefixed index)
                 el))))


(define (path-cycles? path [previous #f] [encountered (set)])
  ; Do not let simplify-path consult filesystem, because that would
  ; follow any link present. We would not get its identity then.
  (define simple (simplify-path path #f))
  (define id (file-or-directory-identity simple #t))
  (cond [(equal? id previous) #f] ; Checks for root directory, given call below.
        [(set-member? encountered id)]
        [else
         (path-cycles? (build-path simple 'up)
                       id
                       (set-add encountered id))]))

(define (build-strictly-deeper-path base-path . els)
  (let ([path-to-verify (simplify-path (apply build-path base-path els))])
    (if (path-prefix? path-to-verify base-path)
        path-to-verify
        #f)))
