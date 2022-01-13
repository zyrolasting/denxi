#lang racket/base

(require racket/contract racket/stream)
(provide (all-from-out racket/stream)
         (contract-out
          [stream-exactly-one
           (-> stream? (-> any) any)]
          [stream-next
           (-> stream? (-> any) any)]))


(module+ test
  (require rackunit racket/function)
  (define has-none empty-stream)
  (define has-one (stream-cons 1 has-none))
  (define has-two (stream-cons 2 has-one))
  (check-pred void? (stream-exactly-one has-none void))
  (check-equal? (stream-exactly-one has-one void) 1)
  (check-pred void? (stream-exactly-one has-two void))

  (check-true (stream-next '(#t) (const #f)))
  (check-false (stream-next '() (const #f))))


(define (stream-exactly-one s fail-thunk)
  (or (and (not (stream-empty? s))
           (stream-empty? (stream-rest s))
           (stream-first s))
      (fail-thunk)))


(define (stream-next s fail-thunk)
  (if (stream-empty? s)
      (fail-thunk)
      (stream-first s)))
