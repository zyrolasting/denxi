#lang racket/base

; Define a monadic type that combines a single value with logged
; messages.

(require (for-syntax racket/base)
         racket/generator
         racket/list
         racket/sequence
         racket/set
         "contract.rkt"
         "exn.rkt"
         "format.rkt"
         "message.rkt"
         "monad.rkt")

(provide (struct-out logged)
         SUCCESS
         FAILURE
         logged-acyclic
         logged-unit
         logged-failure
         logged-attachment
         logged/c
         messy-log/c
         run-log
         get-log
         define-logged
         call-with-logged-continuation
         (contract-out
          [coerce-logged
           (-> any/c logged?)]
          [logged-combine
           (-> logged?
               (-> list? list? list?)
               logged?)]
          [dump-log
           (->* ()
                (#:dump-message (-> $message? any)
                 #:force-value any/c)
                #:rest list?
                (logged/c any/c))]
          [logged-map
           (-> logged?
               (-> $message? $message?)
               logged?)]))

(define SUCCESS (string->uninterned-symbol "SUCCESS"))
(define FAILURE (string->uninterned-symbol "FAILURE"))

(define+provide-message $cycle (key))

(define messy-log/c
  (or/c $message?
        (listof (recursive-contract messy-log/c))))

(struct logged (thnk)
  #:transparent
  #:property prop:procedure (λ (self [m null]) (run-log self m))
  #:methods gen:monad
  [(define (bind a b) (logged-bind a b))
   (define (return M v) (logged-unit v))])

(define (logged-bind st f)
  (logged
   (λ (messages)
     (define-values (next-value new-messages) (run-log st messages))
     (if (or (eq? next-value SUCCESS)
             (eq? next-value FAILURE))
         (values next-value new-messages)
         (run-log (f next-value)
                  new-messages)))))


(define logged-acyclic
  (let ([mark-key (string->uninterned-symbol "xiden:cycle-detection")])
    (λ (key proc)
      (logged
       (λ (messages)
         (let ([scope (or (continuation-mark-set-first (current-continuation-marks) mark-key) (set))])
           (if (set-member? scope key)
               (values FAILURE (cons ($cycle key) messages))
               (with-continuation-mark mark-key (set-add scope key)
                 (proc messages)))))))))


(define (logged-unit v)
  (logged (λ (m) (values v m))))

(define (logged-failure e)
  (logged
   (λ (m)
     (values FAILURE
             (cons (cond [(exn? e) ($show-string (exn->string e))]
                         [($message? e) e]
                         [else ($show-datum e)])
                   m)))))

(define (dump-log #:dump-message [dump-message writeln] #:force-value [v (void)] . preamble)
  (logged
   (λ (messages)
     (sequence-for-each dump-message
                        (sequence-append (in-log-messages preamble)
                                         (in-log-messages messages)))
     (values v messages))))

(define (coerce-logged v)
  (if (logged? v)
      v
      (logged-unit v)))

(define (in-log-messages messages)
  (in-generator
   (let loop ([avail messages])
     (if (null? avail)
         (void)
         (begin (let ([next (car messages)])
                  (if (list? next)
                      (loop next)
                      (yield next)))
                (loop (cdr messages)))))))


(define (call-with-logged-continuation p)
  (logged
   (λ (messages)
     (call/cc
      (λ (return)
        (define (use v [m messages]) (return v m))
        (define (pass [m #f]) (return SUCCESS (if m (cons m messages) messages)))
        (define (fail [m #f]) (return FAILURE (if m (cons m messages) messages)))
        (define (attach v [m #f]) (return v (if m (cons m messages) messages)))
        (define (run! l)  (run-log l messages))
        (p use attach pass fail run! messages))))))


(define-syntax (define-logged stx)
  (syntax-case stx ()
    [(_ (id . formals) body ...)
     (syntax-protect
      (datum->syntax stx
                     (syntax->datum
                      #'(define (id . formals)
                          (call-with-logged-continuation
                           (λ ($use $attach $pass $fail $run! $messages)
                             body ...))))
                     stx))]))


(define (run-log m [messages null])
  (with-handlers ([exn? (λ (e) (run-log (logged-failure e) messages))])
    ((logged-thnk m) messages)))

(define (logged-attachment v next)
  (logged (λ (m) (values v (cons next m)))))

(define (get-log m)
  (define-values (_ messages) (run-log m))
  (reverse (flatten messages)))

; Use to "scope" a selection of messages.
;
; (define pkg-build
;   (logged-map $package
;     (build-package ...)))
;
(define (logged-map l f)
  (logged-combine l (λ (to-map messages)
                      (append (map f to-map)
                              messages))))

(define (logged-combine l f)
  (logged (λ (messages)
            (let-values ([(v to-wrap) (run-log l null)])
              (values v
                      (f to-wrap
                         messages))))))

(define-syntax-rule (logged/c cnt)
  (struct/c logged (-> list? (values (or/c SUCCESS FAILURE cnt) list?))))

(module+ test
  (provide test-logged-procedure)
  (require rackunit)

  (define (test-logged-procedure #:with [initial null] msg l p)
    (test-case msg
      (call-with-values (λ () (run-log l initial)) p)))

  (test-logged-procedure "Check contract on logged procedures"
                         (invariant-assertion (logged/c (>=/c 0)) (logged-unit -7))
                         (λ (p msg)
                           (check-eq? p FAILURE)
                           (check-match msg
                                        (list ($show-string (regexp "assertion violation"))))))

  (test-case "Inject logged program control into lexical context of new procedures"
    (define-logged (try branch)
      (case branch
        [(use-1arg) ($use branch)]
        [(use-2arg) ($use branch branch)]
        [(pass)     ($pass branch)]
        [(fail)     ($fail branch)]
        [(run)      ($run! (logged (λ (m) (values branch (cons branch m)))))]))

    (define (check #:with [base-messages null] logged-proc expected-val expected-messages)
      (test-logged-procedure #:with base-messages
                             (format "Expecting ~s ~s" expected-val expected-messages)
                             logged-proc
                             (λ (actual-value actual-messages)
                               (check-equal? actual-value expected-val)
                               (check-equal? actual-messages expected-messages))))

    (check (try 'use-1arg) #:with '(1) 'use-1arg '(1))
    (check (try 'use-2arg) 'use-2arg 'use-2arg)
    (check (try 'pass) SUCCESS '(pass))
    (check (try 'fail) FAILURE '(fail))
    (check (try 'run) #:with '(1) 'run '(run 1)))

  (test-case "Wrap a selection of logged messages"
    (define-message $wrapper     (v))
    (define-message $wrappable   (v))
    (define-message $unwrappable (v))

    (define will-wrap
      (logged-map (logged (λ (messages)
                            (values #t
                                    (append (build-list 3 $wrappable)
                                            messages))))
                  (λ (m)
                    (check-pred $wrappable? m)
                    ($wrapper m))))

    (call-with-values (λ () (run-log will-wrap (build-list 3 $unwrappable)))
                      (λ (v messages)
                        (check-true v)
                        (check-equal? messages
                                      (list ($wrapper ($wrappable 0))
                                            ($wrapper ($wrappable 1))
                                            ($wrapper ($wrappable 2))
                                            ($unwrappable 0)
                                            ($unwrappable 1)
                                            ($unwrappable 2))))))

  (test-case "Accumulate messages via do notation"
    (define-message $foo (v))
    (define-message $bar (v))
    (define-message $zap (v))

    (define first-step
      (logged (λ (m) (values 1 m))))

    (define (second-step v)
      (logged
       (λ (m)
         (values (* v v)
                      (cons ($foo 1)
                            m)))))

    (define (third-step v)
      (logged (λ (m)
                (values (/ v 3)
                        (cons (list ($zap 3)
                                    ($bar 2))
                              m)))))

    (define action
      (mdo initial := first-step
           next    := (second-step initial)
           final   := (third-step next)
           (logged-unit final)))

    (check-equal? (get-log action)
                  (list ($foo 1)
                        ($bar 2)
                        ($zap 3)))

    (test-equal? "Stop evaluation on request"
                 (get-log (mdo x := (second-step 2)
                               y := (logged (λ (m) (values FAILURE m)))
                               (logged (λ (m) (fail "Should not get here") (values FAILURE null)))))
                 (list ($foo 1)))

    (test-case "Show exceptions in log"
      (define (try makes-error)
        (check-match (get-log (mdo x := (makes-error ((exc exn:fail:xiden) "blah ~a" 'abc))
                                   (logged (λ (m) (fail "Should not get here") (values FAILURE null)))))
                     (list ($show-string (pregexp "blah abc.+")))))

      (test-case "Show caught exceptions"
        (try (λ (e) (logged (λ (m) (raise e))))))))

  (test-case "Cycle detection"
    (define cyclic (logged-acyclic 1 (λ (m) (run-log cyclic m))))
    (define acyclic (logged-acyclic 1 (λ (m) (values 'ok (cons 'ok m)))))

    (test-logged-procedure "Make acyclic procedures behave like normal logged procedures"
                           acyclic
                           (λ (v msg)
                             (check-eq? v 'ok)
                             (check-equal? msg '(ok))))

    (test-logged-procedure "Block cycles in acyclic logged procedures"
                           cyclic
                           (λ (v msg)
                             (check-eq? v FAILURE)
                             (check-equal? msg (list ($cycle 1)))))))
