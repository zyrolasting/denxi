#lang racket/base

(require racket/contract racket/stream)
(provide (all-from-out racket/stream)
         (contract-out
          [stream-consume
           (-> stream? (-> any/c stream? any) (-> any) any)]
          [stream-exactly-one
           (-> stream? (-> any) any)]
          [stream-next
           (-> stream? (-> any) any)]))


(module+ test
  (require racket/function
           "test.rkt")

  (test streams
        (define has-none empty-stream)
        (define has-one (stream-cons 1 has-none))
        (define has-two (stream-cons 2 has-one))
        (assert (void? (stream-exactly-one has-none void)))
        (assert (equal? 1 (stream-exactly-one has-one void)))
        (assert (void? (stream-exactly-one has-two void)))
        (assert (stream-next '(#t) (const #f)))
        (assert (not (stream-next '() (const #f))))

        (assert (stream-consume empty-stream (const #f) (const #t)))
        (stream-consume '(1)
                        (λ (head tail)
                          (assert (equal? head 1))
                          (assert (stream-empty? tail)))
                        (λ () (assert #f)))))


(define (stream-exactly-one s fail-thunk)
  (or (and (not (stream-empty? s))
           (stream-empty? (stream-rest s))
           (stream-first s))
      (fail-thunk)))


(define (stream-next s fail-thunk)
  (if (stream-empty? s)
      (fail-thunk)
      (stream-first s)))

(define (stream-consume s consequent alternate)
  (if (stream-empty? s)
      (alternate)
      (consequent (stream-first s) (stream-rest s))))
