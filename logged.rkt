#lang racket/base

; Define a monadic type that combines a single value with logged
; messages.

(require "contract.rkt")
(provide (struct-out logged)
         SUCCESS
         FAILURE
         Logged
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
          [logged-combine
           (-> logged?
               (-> list? list? list?)
               logged?)]

          [logged-map
           (-> logged?
               (-> $message? $message?)
               logged?)]))

(require racket/list
         "exn.rkt"
         "format.rkt"
         "message.rkt"
         "monad.rkt"
         (for-syntax racket/base))

(define SUCCESS (string->uninterned-symbol "SUCCESS"))
(define FAILURE (string->uninterned-symbol "FAILURE"))

(define messy-log/c
  (or/c $message?
        (listof (recursive-contract messy-log/c))))

(struct logged (thnk)
  #:transparent
  #:property prop:procedure (λ (self [m null]) (run-log self m))
  #:methods gen:monad [(define (monad->monad-class m) Logged)])

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


; Like State, except execution can be halted and the state is always a
; list of $message instances.
(define-monad-class Logged
  (λ (st f)
    (logged
     (λ (messages)
       (define-values (next-value new-messages) (run-log st messages))
       (if (or (eq? next-value SUCCESS)
               (eq? next-value FAILURE))
           (values next-value new-messages)
           (run-log (f next-value)
                    new-messages)))))
  logged-unit
  #:fail logged-failure)


(define (run-log m [messages null])
  (with-handlers ([exn? (λ (e) (run-log (logged-failure e) messages))])
    ((logged-thnk (Logged m)) messages)))

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
      (do initial <- first-step
          next    <- (second-step initial)
          final   <- (third-step next)
          (return final)))

    (check-equal? (get-log action)
                  (list ($foo 1)
                        ($bar 2)
                        ($zap 3)))

    (test-equal? "Stop evaluation on request"
                 (get-log (do x <- (second-step 2)
                              y <- (logged (λ (m) (values FAILURE m)))
                              z <- (logged (λ (m) (fail "Should not get here") (values FAILURE null)))
                              (return z)))
                 (list ($foo 1)))

    (test-case "Show exceptions in log"
      (define (try makes-error)
        (check-match (get-log (do x <- (makes-error ((exc exn:fail:xiden) "blah ~a" 'abc))
                                  z <- (logged (λ (m) (fail "Should not get here") (values FAILURE null)))
                                  (return z)))
                     (list ($show-string (pregexp "blah abc.+")))))

      (test-case "Show returned exceptions"
        (try Logged))

      (test-case "Show caught exceptions"
        (try (λ (e) (logged (λ (m) (raise e)))))))))
