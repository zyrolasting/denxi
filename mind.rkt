#lang racket/base

(require racket/contract)
(provide mind-implementation/c
         gen:mind
         (contract-out
          [mind-knowns
           (-> mind? (machine/c sequence?))]
          [mind-recall
           (-> mind? string? (-> known? (machine/c void?)) (machine/c known?))]
          [mind-forget
           (-> mind? any/c (machine/c exact-nonnegative-integer?))]
          [mind-clean
           (-> mind? (machine/c exact-nonnegative-integer?))]))


(require racket/generic
         racket/sequence
         "function.rkt"
         "known.rkt"
         "machine.rkt"
         "monad.rkt"
         "port.rkt"
         "source.rkt")


(define-generics mind
  [mind-recall mind key fail]
  [mind-forget mind key]
  [mind-knowns mind])


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
    (check-machine-value (mind-forget m #"a") 0)))
