#lang racket/base

(require racket/contract)
(provide gen:known
         (contract-out
          [known-implementation/c
           contract?]
          [know
           (->* () ((listof string?) bytes?) known?)]
          [known? predicate/c]
          [known-put-names
           (-> known? (listof string?) (machine/c void?))]
          [known-get-names
           (-> known? (machine/c (listof string?)))]
          [known-put-bytes
           (-> known? input-port? (machine/c void?))]
          [known-open-bytes
           (-> known? (machine/c input-port?))]
          [known-size
           (-> known? (machine/c exact-nonnegative-integer?))]))


(require racket/generic
         racket/port
         racket/stream
         "machine.rkt"
         "message.rkt"
         "monad.rkt")


(define-generics known
  [known-put-names known name]
  [known-get-names known]
  [known-put-bytes known in]
  [known-open-bytes known]
  [known-size known])


(define known-implementation/c
  (known/c [known-put-names
            (-> known? (listof string?) (machine/c void?))]
           [known-get-names
            (-> known? (machine/c (listof string?)))]
           [known-put-bytes
            (-> known? input-port? (machine/c void?))]
           [known-open-bytes
            (-> known? (machine/c input-port?))]
           [known-size
            (-> known? (machine/c exact-nonnegative-integer?))]))


(define (know [aliases null] [data #""])
  (memory-known aliases data))


(struct memory-known (aliases data)
  #:mutable
  #:methods gen:known
  [(define (known-get-names k)
     (machine-unit (memory-known-aliases k)))

   (define (known-put-names k names)
     (machine-unit (set-memory-known-aliases! k names)))

   (define (known-put-bytes k external)
     (machine
      (λ (state)
        (define to-bytes (open-output-bytes))
        (copy-port external to-bytes)
        (flush-output to-bytes)
        (set-memory-known-data! k (get-output-bytes to-bytes #t))
        (close-output-port to-bytes)
        (state-set-value state (void)))))

   (define (known-open-bytes k)
     (machine-unit (open-input-bytes (memory-known-data k))))

   (define (known-size k)
     (machine-unit (bytes-length (memory-known-data k))))])


(define (known-get-bytes known)
  (mdo i := (known-open-bytes known)
       (machine-unit (port->bytes i))))


(module+ test
  (require rackunit
           (submod "machine.rkt" test))

  (test-case "In-memory known"
    (define k (know null #"initial"))
    (define alias "A")

    (check-machine-value (known-put-names k (list alias))
                         (? void? _))

    (check-machine-value (known-get-names k)
                         (list alias))

    (check-machine-value (known-get-bytes k) #"initial")

    (check-machine-value (known-put-bytes k (open-input-string alias))
                         (? void? _))

    (check-machine-value (known-get-bytes k) #"A")

    (check-machine-value (known-size k) 1)))
