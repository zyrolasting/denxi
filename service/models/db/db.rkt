#lang racket/base

(provide connect-db
         query-rows/apply
         (all-from-out db))

(require db
         racket/runtime-path)

(define-runtime-path db.sqlite3 "db.sqlite3")

(define (query-rows/apply conn proc . args)
  (map (λ (row) (apply proc (vector->list row)))
       (apply query-rows conn args)))

(define (connect-db)
  (define conn
    (sqlite3-connect #:database db.sqlite3
                     #:mode 'create))
  (λ (cmd . others)
    (cond [(procedure? cmd)
           (apply cmd conn others)]
          [(eq? 'quit cmd)
           (disconnect conn)])))
