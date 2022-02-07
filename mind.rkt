#lang racket/base

(require racket/contract)
(provide mind-implementation/c
         gen:mind
         (contract-out
          [mind-knowns
           (-> mind? (machine/c sequence?))]
          [mind-recall
           (-> mind? string? (-> output-port? (machine/c void?))
               (machine/c known?))]))


(require racket/generic
         racket/sequence
         "function.rkt"
         "known.rkt"
         "machine.rkt"
         "monad.rkt"
         "port.rkt"
         "source.rkt")


(define-generics mind
  [mind-recall mind key fill-bytes exchange-names]
  [mind-knowns mind])


(struct filesystem-mind (directory-path)
  #:methods
  [(define (mind-recall mind key fill-bytes exchange-names)
     (machine
      (λ (state)
        (define path (filesystem-mind-path mind key))
        (unless (file-exists? path)
          (call-with-output-file path fill-bytes))
        (define names (exchange-names))
        names)))

   (define (mind-forget mind key)
     (machine
      (λ (state)
        (delete-file key)
        state)))

   (define (mind-knowns mind)
     (machine
      (λ (state)
        (in-generator #:arity 2
                      (for ([path (in-list (directory-list (filesystem-mind-directory-path mind)))])
                        (yield path (file-known path)))))))])


(define (filesystem-mind-path mind path)
  (build-path (filesystem-mind-directory-path mind)
              path))



(define mind-implementation/c
  (mind/c [mind-recall (-> mind? bytes? (-> known?) known?)]
          [mind-knowns (-> mind? bytes? (machine/c sequence?))]
          [mind-forget (-> mind? bytes? (machine/c exact-nonnegative-integer?))]))


(define (mind-clean mind)
  (machine
   (λ (state)
     (define state* ((mind-knowns mind) state))
     (define knowns (state-get-value state*))
     (define (collect key known)
       (mdo names := (known-get-names known)
            (if (null? names)
                (mdo size := (mind-forget mind key)
                     (machine
                      (λ (state)
                        (state-set-value state
                                         (+ size (state-get-value state))))))
                (machine values))))
     (sequence-fold (λ (s m) (m s))
                    (state-set-value state* 0)
                    (sequence-map (curry collect mind) knowns)))))     


(define (intraprocess-mind)
  (memory-mind (make-hash)))


(struct memory-mind (knowns)
  #:methods gen:mind
  [(define (mind-recall mind key learn)
     (machine
      (λ (state)
        (define table (memory-mind-knowns mind))
        (if (hash-has-key? table key)
            (state-set-value state (hash-ref table key))
            (let ([k (know)])
              (hash-set! table key k)
              ((learn k) state))))))


   (define (mind-forget mind key)
     (define known (hash-ref (memory-mind-knowns mind) key #f))
     (if known
         (mdo size := (known-size known)
              (machine
               (λ (state)
                 (hash-remove! (memory-mind-knowns mind) key)
                 state)))
         (machine-unit 0)))


   (define (mind-knowns mind)
     (machine-unit (in-hash (memory-mind-knowns mind))))])


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
  (test-case "Intraprocess mind"
    (define m (intraprocess-mind))
    (define recall (mind-recall m #"a" machine-unit))
    (define k (state-get-value (recall)))
    (check-eq? (state-get-value (recall)) k)
    (void ((known-put-bytes k (open-input-bytes #"xyz"))))
    (check-equal? (sequence->list (sequence-map list (car ((mind-knowns m)))))
                  (list (list #"a" k)))
    (check-machine-value (mind-forget m #"a") 3)
    (check-machine-value (mind-forget m #"a") 0))

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
