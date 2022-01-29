#lang racket/base

(require racket/contract)
(provide gen:known
         (contract-out
          [know
           (->* () ((listof string?) bytes?) known?)]
          [known? predicate/c]
          [known-put-names
           (-> known? (listof string?) (subprogram/c void?))]
          [known-get-names
           (-> known? (subprogram/c (listof string?)))]
          [known-put-bytes
           (-> known? input-port? (subprogram/c void?))]
          [known-open-bytes
           (-> known? (subprogram/c input-port?))]
          [known-size
           (-> known? (subprogram/c exact-nonnegative-integer?))]))


(require racket/generic
         racket/port
         racket/stream
         "monad.rkt"
         "subprogram.rkt")


(define-generics known
  [known-put-names known name]
  [known-get-names known]
  [known-put-bytes known in]
  [known-open-bytes known]
  [known-size known])


(define (know [aliases null] [data #""])
  (memory-known aliases data))


(struct memory-known (aliases data)
  #:mutable
  #:methods gen:known
  [(define (known-get-names k)
     (subprogram-unit (memory-known-aliases k)))

   (define (known-put-names k names)
     (subprogram-unit (set-memory-known-aliases! k names)))

   (define (known-put-bytes k external)
     (subprogram
      (λ (messages)
        (define to-bytes (open-output-bytes))
        (copy-port external to-bytes)
        (flush-output to-bytes)
        (set-memory-known-data! k (get-output-bytes to-bytes #t))
        (close-output-port to-bytes)
        (values (void) messages))))

   (define (known-open-bytes k)
     (subprogram-unit (open-input-bytes (memory-known-data k))))

   (define (known-size k)
     (subprogram-unit (bytes-length (memory-known-data k))))])


(define (known-get-bytes known)
  (mdo i := (known-open-bytes known)
       (subprogram-unit (port->bytes i))))


(module+ test
  (require rackunit
           (submod "subprogram.rkt" test))

  (test-case "In-memory known"
    (define k (know null #"initial"))
    (define alias "A")
    (define run get-subprogram-value)

    (check-pred void? (run (known-put-names k (list alias))))
    (check-equal? (run (known-get-names k))
                  (list alias))

    (check-equal? (run (known-get-bytes k)) #"initial")
    (check-pred void? (run (known-put-bytes k (open-input-string alias))))
    (check-equal? (get-subprogram-value (known-get-bytes k)) #"A")

    (check-equal? (get-subprogram-value (known-size k)) 1)))
