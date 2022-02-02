#lang racket/base

(require racket/contract)
(provide mind-implementation/c
         gen:mind
         (contract-out
          [mind-knowns
           (-> mind? (subprogram/c sequence?))]
          [mind-recall
           (-> mind? string? (-> known? (subprogram/c void?)) (subprogram/c known?))]
          [mind-forget
           (-> mind? any/c (subprogram/c exact-nonnegative-integer?))]
          [mind-clean
           (-> mind? (subprogram/c exact-nonnegative-integer?))]))



(require racket/generic
         racket/sequence
         "function.rkt"
         "known.rkt"
         "monad.rkt"
         "port.rkt"
         "source.rkt"
         "subprogram.rkt")


(define-generics mind
  [mind-recall mind key fail]
  [mind-forget mind key]
  [mind-knowns mind])


(define mind-implementation/c
  (mind/c [mind-recall (-> mind? bytes? (-> known?) known?)]
          [mind-knowns (-> mind? bytes? (subprogram/c sequence?))]
          [mind-forget (-> mind? bytes? (subprogram/c exact-nonnegative-integer?))]))



(define (mind-clean mind)
  (mdo knowns := (mind-knowns mind)
       (subprogram-fold (subprogram-unit 0)
                        (sequence-map (curry collect mind) knowns))))


(define (intraprocess-mind)
  (memory-mind (make-hash)))


(struct memory-mind (knowns)
  #:methods gen:mind
  [(define (mind-recall mind key learn)
     (subprogram
      (λ (messages)
        (define table (memory-mind-knowns mind))
        (if (hash-has-key? table key)
            (values (hash-ref table key)
                    messages)
            (let*-values ([(k) (know)]
                          [(value messages*) (run-subprogram (learn k) messages)])
              (hash-set! table key k)
              (values k messages*))))))

   (define (mind-forget mind key)
     (define known (hash-ref (memory-mind-knowns mind) key #f))
     (if known
         (mdo size := (known-size known)
              (subprogram
               (λ (messages)
                 (hash-remove! (memory-mind-knowns mind) key)
                 (values size messages))))
         (subprogram-unit 0)))


   (define (mind-knowns mind)
     (subprogram-unit (in-hash (memory-mind-knowns mind))))])


(define ((collect mind key known) accumulated)
  (mdo names := (known-get-names known)
       (if (null? names)
           (mdo size := (mind-forget mind key)
                (subprogram-unit (+ size accumulated)))
           (subprogram-unit accumulated))))


(module+ test
  (require rackunit
           (submod "subprogram.rkt" test))
  (test-case "Intraprocess mind"
    (define m (intraprocess-mind))
    (define k (get-subprogram-value (mind-recall m #"a" subprogram-unit)))
    (check-eq? (get-subprogram-value (mind-recall m #"a" subprogram-unit)) k)
    (get-subprogram-value (known-put-bytes k (open-input-bytes #"xyz")))
    (check-equal? (sequence->list (sequence-map list (get-subprogram-value (mind-knowns m))))
                  (list (list #"a" k)))
    (check-equal? (get-subprogram-value (mind-forget m #"a")) 3)
    (check-equal? (get-subprogram-value (mind-forget m #"a")) 0)))
