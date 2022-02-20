#lang racket/base

#|
Define an abstract machine of two registers as a monadic type.  The
machine's state S consists of both registers, expressed as a Racket
list.

(car S) is any Racket value. Defaults to `undefined`, from
`racket/undefined`. If set to `halt`, then the machine will not
transition to any new state.

(cdr S) is a list of $message instances that contextualize (car S).

If any Racket code raises a (negate exn:break?) value in the context
of a machine, then the machine will transition to the `halt` state and
add a serializable encoding of the exception to (cdr S) via cons.

To define a machine, wrap (machine) around a unary Racket procedure.
The procedure must return a new state via functional update. For
example, this machine sets the first register to 1. Each machine
presents as a Racket procedure, and can be called with or without an
existing state.

  (define set-one (machine (lambda (state) (state-set-value state 1))))
  (state-get-value (set-one)) ; 1

Combine machines with the `mdo` (monadic do) form.

  (define-message $foo (a b c))
  (define set-one (machine (lambda (state) (state-set-value state 1))))
  (define log-foo (machine (lambda (state) (state-add-message state ($foo 1 2 3)))))
  (define composite (mdo set-one log-foo))
  (composite)
|#


(require racket/contract
         racket/function
         racket/sequence
         racket/set
         racket/undefined
         "message.rkt"
         "monad.rkt")

(provide machine-effect
         machine-rule
         (struct-out machine)
         (contract-out
          [halt
           symbol?]
          [state-undefined
           state-undefined?]
          [machine/c
           (case-> (-> contract? contract?)
                   (-> contract? contract? contract?))]
          [machine-bind
           (-> machine? (-> any/c machine?) machine?)]
          [machine-coerce
           (-> any/c machine?)]
          [machine-failover
           (-> machine? machine? machine?)]
          [machine-halt-with
           (-> any/c machine?)]
          [machine-unit
           (-> any/c machine?)]
          [machine-series
           (-> (sequence/c machine?) machine?)]
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
          [state-get-value
           (-> state-like? any/c)]
          [state-get-messages
           (-> state-like? list?)]
          [state-set-value
           (-> state-like? any/c state-like?)]))


(define halt
  (string->uninterned-symbol "halt"))


(define (state/c value/c)
  (cons/c value/c (listof $message?)))


(define state-undefined
  (list undefined))


(define (state-get-value state)
  (car state))


(define (state-get-messages state)
  (cdr state))


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


(define machine/c
  (case-lambda [(range/c)
                (machine/c any/c range/c)]
               [(domain/c range/c)
                (struct/c machine (-> (state/c domain/c) (state/c range/c)))]))


(define-syntax-rule (machine-effect x)
  (machine (λ (s) x s)))


(define-syntax-rule (machine-rule x)
  (machine (λ (s) (state-set-value s x))))


(define (machine-series machines)
  (machine
   (λ (state)
     (for/fold ([state* state])
               ([m machines] #:break (state-halt? state*))
       (m state*)))))


(define (machine-bind m f)
  (machine
   (λ (state)
     (if (state-halt? state)
         state
         (let ([state* (m state)])
           (if (state-halt? state*)
               state*
               ((f (car state*)) state*)))))))


(define (machine-failover m m*)
  (machine
   (λ (state)
     (let ([state* (m state)])
       (if (state-halt? state*)
           (m* (state-set-value state* (state-get-value state)))
           state*)))))


(define (machine-unit v)
  (machine (λ (state) (state-set-value state v))))


(define (machine-halt-with e)
  (machine (λ (state) (state-halt-with state e))))


(define (machine-coerce v)
  (if (machine? v)
      v
      (machine-unit v)))


(define (machine-or ? . machines)
  (machine
   (λ (state)
     (for/fold ([state* state])
               ([m machines]
                #:break (? (state-get-value state*)))
       (m state*)))))


(module+ test
  (provide match-halt?)
  (require "test.rkt")

  (define-syntax-rule (match-halt? s . patt)
    (match? s (list (? (curry eq? halt) _)
                    . patt)))

  (test machine-basics
        (assert (state-halt? (list halt)))
        (assert (not (state-halt? (list 1))))
        (assert (equal? (state-set-value '(#f) #t) '(#t)))
        (assert (match-halt? ((invariant-assertion (machine/c any/c (>=/c 0)) (machine-unit -1)) state-undefined)
                             ($show-string (regexp "assertion violation")))))

  (test machine-do-notation
    (define-message $initial (v))

    (define step1
      (machine-bind
       (machine-unit ($initial 1))
       (λ (value) (machine-unit (add1 ($initial-v value))))))

    (define (step2 v)
      (machine
       (λ (state)
         (assert (equal? (car state) 2))
         (state-set-value state (* v v)))))

    (assert (equal? ((mdo initial := step1
                          next    := (step2 initial)
                          (machine-unit next)))
                    '(4)))

    (assert (match? ((mdo x := (machine-unit 2)
                          y := (machine-halt-with 1)
                          (machine (λ _ (error "Should not get here")))))
                    (list halt ($show-string "1")))))

  (test failover
        (assert (equal? ((machine-failover (machine-halt-with 1)
                                           (machine-unit 2)))
                        (list 2 ($show-string "1"))))))
