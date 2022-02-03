#lang racket/base

(require racket/contract
         racket/function
         racket/set
         racket/undefined
         "message.rkt"
         "monad.rkt")

(provide (struct-out $cycle)
         (struct-out machine)
         (contract-out
          [halt
           symbol?]
          [state-undefined
           state-undefined?]
          [machine/c
           (-> contract? contract? contract?)]
          [machine-acyclic
           (-> procedure? any/c machine?)]
          [machine-bind
           (-> machine? (-> any/c machine?) machine?)]
          [machine-coerce
           (-> any/c machine?)]
          [machine-halt-with
           (-> any/c machine?)]
          [machine-unit
           (-> any/c machine?)]
          [state/c
           contract?]
          [state-add-message
           (-> state-like? $message? state-like?)]
          [state-undefined?
           predicate/c]
          [state-halt?
           predicate/c]
          [state-halt-with
           (-> state-like? any/c state-halt?)]
          [state-set-value
           (-> state-like? any/c state-like?)]))

(define halt
  (string->uninterned-symbol "halt"))


(define (state/c value/c)
  (cons/c value/c (listof $message?)))


(define state-undefined
  (list undefined))


(define (state-like? v)
  (and (list? v)
       (not (null? v))))


(define (state-halt? state)
  (and (state-like? state)
       (eq? halt (car state))))


(define (state-undefined? state)
  (and (state-like? state)
       (eq? undefined (car state))))


(define (state-set-value state v)
  (cons v (cdr state)))


(define (state-add-message state v)
  (cons (car state)
        (cons v (cdr state))))


(define (state-halt-with state v)
  (state-add-message (state-set-value state halt)
                     (coerce-$message v)))


(struct machine (procedure)
  #:property prop:procedure
  (λ (self [state state-undefined])
    (with-handlers ([(negate exn:break?) (curry state-halt-with state)])
      ((machine-procedure self) state)))
  #:methods gen:monad
  [(define (bind a b) (machine-bind a b))
   (define (return M v) (machine-unit v))])


(define (machine/c domain/c range/c)
  (struct/c machine (-> (state/c domain/c) (state/c range/c))))


(define (machine-bind m f)
  (machine
   (λ (state)
     (if (state-halt? state)
         state
         (let ([state* (m state)])
           (if (state-halt? state*)
               state*
               ((f (car state*)) state*)))))))


(define-message $cycle (key))
(define machine-acyclic
  (let ([mark-key (string->uninterned-symbol "denxi:cycle-detection")])
    (λ (key proc)
      (machine
       (λ (state)
         (let ([scope (or (continuation-mark-set-first (current-continuation-marks) mark-key) (set))])
           (if (set-member? scope key)
               (state-halt-with state ($cycle key))
               (with-continuation-mark mark-key (set-add scope key)
                 (proc state)))))))))


(define (machine-unit v)
  (machine (λ (state) (state-set-value state v))))


(define (machine-halt-with e)
  (machine (λ (state) (state-halt-with state e))))


(define (machine-coerce v)
  (if (machine? v)
      v
      (machine-unit v)))


(module+ test
  (require rackunit)

  (provide check-machine-state
           check-machine-value
           check-machine-messages
           check-machine-halt
           check-machine-contract-violation)

  (define-syntax-rule (check-machine-state m pattern)
    (check-match (m) pattern))

  (define-syntax-rule (check-machine-value m pattern)
    (check-match (m) (cons pattern (list _ ___))))

  (define-syntax-rule (check-machine-messages m . patterns)
    (check-match (m) (cons _ (list . patterns))))

  (define-syntax-rule (check-machine-halt m . patterns)
    (check-match (m) (cons (? (curry eq? halt) _) (list . patterns))))

  (define (check-machine-contract-violation domain/c range/c m s)
    (check-match ((invariant-assertion (machine/c domain/c range/c) m) s)
                 (list (? (curry eq? halt) _)
                       ($show-string (regexp "assertion violation")))))

  (check-true (state-halt? (list halt)))
  (check-false (state-halt? (list 1)))

  (check-equal? (state-set-value '(#f) #t) '(#t))

  (check-machine-contract-violation any/c
                                    (>=/c 0)
                                    (machine-unit -1)
                                    state-undefined)

  (test-case "Use machines in do notation"
    (define-message $initial (v))

    (define step1
      (machine-bind
       (machine-unit ($initial 1))
       (λ (value) (machine-unit (add1 ($initial-v value))))))

    (define (step2 v)
      (machine
       (λ (state)
         (check-equal? (car state) 2)
         (state-set-value state (* v v)))))

    (check-equal? ((mdo initial := step1
                        next    := (step2 initial)
                        (machine-unit next)))
                  '(4))

    (check-machine-halt (mdo x := (machine-unit 2)
                             y := (machine-halt-with 1)
                             (machine (λ _ (fail "Should not get here"))))
                        ($show-string "1"))

  (test-case "Cycle detection"
    (define cyclic  (machine-acyclic 'cycles (λ (s) (cyclic s))))
    (define acyclic (machine-acyclic 'cycles (λ (s) (state-set-value s 'ok))))
    (check-machine-value acyclic 'ok)
    (check-machine-halt cyclic ($cycle 'cycles)))))
