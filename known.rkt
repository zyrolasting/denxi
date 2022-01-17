#lang s-exp "base.rkt"

(provide gen:known
         (contract-out
          [known? predicate/c]
          [known-elsewhere?
           (-> known? boolean?)]
          [known-here?
           (-> known? boolean?)]
          [known-add-name
           (-> known? string? (-> any) any)]
          [known-by
           (-> known? (-> any) any)]
          [known-open-output
           (-> known? (subprogram/c
                       (case-> (-> input-port? transfer-policy? void?)
                               (-> void?))))]
          [known-open-input
           (-> known? (subprogram/c
                       (case-> (-> output-port? transfer-policy? void?)
                               (-> void?))))]))


(require racket/generic
         racket/stream
         "monad.rkt"
         "port.rkt"
         "subprogram.rkt")


(module+ test
  (require rackunit
           (submod "subprogram.rkt" test))

  (test-case "In-memory known"
    (define k (make-memory-known #f null #"initial"))
    (define canonical-name "_")
    (define alias "A")
    (define (added)
      (stream->list (known-by k fail)))

    (check-false (known-elsewhere? k))

    (known-add-name k canonical-name fail)
    (check-false (known-elsewhere? k))
    (check-equal? (added) (list canonical-name))

    (known-add-name k alias fail)
    (check-true (known-elsewhere? k))
    (check-equal? (added) (list canonical-name alias))

    
    (check-subprogram (mdo f := (known-open-input k)
                           (let-values ([(i o) (make-pipe)])
                             (f o full-trust-transfer-policy)
                             (f)
                             (subprogram-unit (port->bytes i))))
                      (λ (bstr)
                        (check-equal? bstr #"initial")))
    
    
    (check-subprogram (mdo f := (known-open-output k)
                           (begin0 (subprogram-unit (f (open-input-string alias)
                                                       full-trust-transfer-policy))
                             (f)))
                      (λ (v)
                        (check-equal? v alias)))))



(define-generics known
  [known-add-name known name fail]
  [known-by known fail]
  [known-open-output known]
  [known-open-input known])


(define (known-elsewhere? known)
  (define s (known-by known (λ () empty-stream)))
  (and (not (stream-empty? s))
       (not (stream-empty? (stream-rest s)))))


(define (known-here? known)
  (define s (known-by known (λ () empty-stream)))
  (not (stream-empty? s)))


(define (make-memory-known canonical-name [aliases null] [data #""])
  (memory-known canonical-name aliases data (make-semaphore 1)))


(struct memory-known (name aliases data semaphore)
  #:mutable
  #:methods gen:known
  [(define (known-by k fail)
     (if (memory-known-name k)
         (in-list (cons (memory-known-name k)
                        (memory-known-aliases k)))
         (fail)))

   (define (known-add-name k name fail)
     (if (memory-known-name k)
         (set-memory-known-aliases! k (cons name (memory-known-aliases k)))
         (set-memory-known-name! k name)))

   (define (known-open-output k)
     (subprogram
      (λ (value messages)
        (define semaphore (memory-known-semaphore k))
        (semaphore-wait semaphore)
        (define-values (i o) (make-pipe))
        (values (case-lambda [()
                              (flush-output o)
                              (close-output-port o)                              
                              (set-memory-known-data! k (port->bytes i))
                              (semaphore-post semaphore)]
                             [(in policy)
                              (transfer in o policy)])
                messages))))

   (define (known-open-input k)
     (subprogram
      (λ (value messages)
        (define semaphore (memory-known-semaphore k))
        (semaphore-wait semaphore)
        (define i (open-input-bytes (memory-known-data k)))
        (values (case-lambda [()
                              (close-input-port i)
                              (semaphore-post semaphore)]
                             [(o policy)
                              (transfer i o policy)])
                messages))))])



(define-subprogram (replace-known-bytes k continue)
  (mdo << := (known-open-output k)
       (dynamic-wind void
                     (λ () (continue <<))
                     <<)))
