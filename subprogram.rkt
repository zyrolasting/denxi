#lang racket/base

; Define a monadic type that combines a single value with subprogram
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

(provide (struct-out subprogram)
         FAILURE
         subprogram-acyclic
         subprogram-unit
         subprogram-failure
         subprogram-attachment
         subprogram/c
         subprogram-log/c
         run-subprogram
         get-subprogram-value
         get-subprogram-log
         define-subprogram
         call-with-subprogram-continuation
         (contract-out
          [coerce-subprogram
           (-> any/c subprogram?)]
          [subprogram-combine
           (-> subprogram?
               (-> list? list? list?)
               subprogram?)]
          [dump-subprogram
           (->* ()
                (#:dump-message (-> $message? any)
                 #:force-value any/c)
                #:rest list?
                (subprogram/c any/c))]
          [subprogram-map
           (-> subprogram?
               (-> $message? $message?)
               subprogram?)]))

(define FAILURE (string->uninterned-symbol "FAILURE"))

(define+provide-message $cycle (key))

(define subprogram-log/c
  (or/c $message?
        (listof (recursive-contract subprogram-log/c))))

(struct subprogram (thnk)
  #:transparent
  #:property prop:procedure (λ (self [m null]) (run-subprogram self m))
  #:methods gen:monad
  [(define (bind a b) (subprogram-bind a b))
   (define (return M v) (subprogram-unit v))])

(define (subprogram-bind st f)
  (subprogram
   (λ (messages)
     (define-values (next-value new-messages) (run-subprogram st messages))
     (if (eq? next-value FAILURE)
         (values next-value new-messages)
         (run-subprogram (f next-value)
                         new-messages)))))


(define subprogram-acyclic
  (let ([mark-key (string->uninterned-symbol "xiden:cycle-detection")])
    (λ (key proc)
      (subprogram
       (λ (messages)
         (let ([scope (or (continuation-mark-set-first (current-continuation-marks) mark-key) (set))])
           (if (set-member? scope key)
               (values FAILURE (cons ($cycle key) messages))
               (with-continuation-mark mark-key (set-add scope key)
                 (proc messages)))))))))


(define (subprogram-unit v)
  (subprogram (λ (m) (values v m))))

(define (subprogram-failure e)
  (subprogram
   (λ (m)
     (values FAILURE
             (cons (cond [(exn? e) ($show-string (exn->string e))]
                         [($message? e) e]
                         [else ($show-datum e)])
                   m)))))

(define (dump-subprogram #:dump-message [dump-message writeln] #:force-value [v (void)] . preamble)
  (subprogram
   (λ (messages)
     (sequence-for-each dump-message
                        (sequence-append (in-subprogram-messages preamble)
                                         (in-subprogram-messages messages)))
     (values v messages))))

(define (coerce-subprogram v)
  (if (subprogram? v)
      v
      (subprogram-unit v)))

(define (in-subprogram-messages messages)
  (in-generator
   (let loop ([avail messages])
     (if (null? avail)
         (void)
         (begin (let ([next (car messages)])
                  (if (list? next)
                      (loop next)
                      (yield next)))
                (loop (cdr messages)))))))


(define (call-with-subprogram-continuation p)
  (subprogram
   (λ (messages)
     (call/cc
      (λ (return)
        (define (use v [m messages]) (return v m))
        (define (fail [m #f]) (return FAILURE (if m (cons m messages) messages)))
        (define (attach v [m #f]) (return v (if m (cons m messages) messages)))
        (define (run! l)  (run-subprogram l messages))
        (p use attach fail run! messages))))))


(define-syntax (define-subprogram stx)
  (syntax-case stx ()
    [(_ (id . formals) body ...)
     (syntax-protect
      (datum->syntax stx
                     (syntax->datum
                      #'(define (id . formals)
                          (call-with-subprogram-continuation
                           (λ ($use $attach $fail $run! $messages)
                             body ...))))
                     stx))]))


(define (run-subprogram m [messages null])
  (with-handlers ([exn? (λ (e) (run-subprogram (subprogram-failure e) messages))])
    ((subprogram-thnk m) messages)))

(define (subprogram-attachment v next)
  (subprogram (λ (m) (values v (cons next m)))))

(define (get-subprogram-value l)
  (define-values (v _) (run-subprogram l))
  v)

(define (get-subprogram-log m)
  (define-values (_ messages) (run-subprogram m))
  (reverse (flatten messages)))

; Use to "scope" a selection of messages.
;
; (define pkg-build
;   (subprogram-map $package
;     (build-package ...)))
;
(define (subprogram-map l f)
  (subprogram-combine l (λ (to-map messages)
                          (append (map f to-map)
                                  messages))))

(define (subprogram-combine l f)
  (subprogram (λ (messages)
                (let-values ([(v to-wrap) (run-subprogram l null)])
                  (values v
                          (f to-wrap
                             messages))))))

(define-syntax-rule (subprogram/c cnt)
  (struct/c subprogram (-> list? (values (or/c FAILURE cnt) list?))))

(module+ test
  (require rackunit)
  (provide test-subprogram-procedure
           check-subprogram-procedure
           test-subprogram-value
           check-subprogram-value)
  
  (define (test-subprogram-value msg l check)
    (test-case msg
      (check-subprogram-value l check)))

  (define (check-subprogram-value l check)
    (check-subprogram-procedure l
                                (λ (result messages)
                                  (check-pred null? messages)
                                  (check result))))

  (define (check-subprogram-procedure #:with [initial null] l p)
    (call-with-values (λ () (run-subprogram l initial)) p))
  
  (define (test-subprogram-procedure #:with [initial null] msg l p)
    (test-case msg (check-subprogram-procedure #:with initial l p)))

  (test-subprogram-procedure "Check contract on subprogram procedures"
                             (invariant-assertion (subprogram/c (>=/c 0)) (subprogram-unit -7))
                             (λ (p msg)
                               (check-eq? p FAILURE)
                               (check-match msg
                                            (list ($show-string (regexp "assertion violation"))))))

  (test-case "Inject subprogram program control into lexical context of new procedures"
    (define-subprogram (try branch)
      (case branch
        [(use-1arg) ($use branch)]
        [(use-2arg) ($use branch branch)]
        [(fail)     ($fail branch)]
        [(run)      ($run! (subprogram (λ (m) (values branch (cons branch m)))))]))

    (define (check #:with [base-messages null] subprogram-proc expected-val expected-messages)
      (test-subprogram-procedure #:with base-messages
                                 (format "Expecting ~s ~s" expected-val expected-messages)
                                 subprogram-proc
                                 (λ (actual-value actual-messages)
                                   (check-equal? actual-value expected-val)
                                   (check-equal? actual-messages expected-messages))))

    (check (try 'use-1arg) #:with '(1) 'use-1arg '(1))
    (check (try 'use-2arg) 'use-2arg 'use-2arg)
    (check (try 'fail) FAILURE '(fail))
    (check (try 'run) #:with '(1) 'run '(run 1)))

  (test-case "Wrap a selection of subprogram messages"
    (define-message $wrapper     (v))
    (define-message $wrappable   (v))
    (define-message $unwrappable (v))

    (define will-wrap
      (subprogram-map (subprogram (λ (messages)
                                    (values #t
                                            (append (build-list 3 $wrappable)
                                                    messages))))
                      (λ (m)
                        (check-pred $wrappable? m)
                        ($wrapper m))))

    (call-with-values (λ () (run-subprogram will-wrap (build-list 3 $unwrappable)))
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
      (subprogram (λ (m) (values 1 m))))

    (define (second-step v)
      (subprogram
       (λ (m)
         (values (* v v)
                 (cons ($foo 1)
                       m)))))

    (define (third-step v)
      (subprogram (λ (m)
                    (values (/ v 3)
                            (cons (list ($zap 3)
                                        ($bar 2))
                                  m)))))

    (define action
      (mdo initial := first-step
           next    := (second-step initial)
           final   := (third-step next)
           (subprogram-unit final)))

    (check-equal? (get-subprogram-log action)
                  (list ($foo 1)
                        ($bar 2)
                        ($zap 3)))

    (test-equal? "Stop evaluation on request"
                 (get-subprogram-log (mdo x := (second-step 2)
                                          y := (subprogram (λ (m) (values FAILURE m)))
                                          (subprogram (λ (m) (fail "Should not get here") (values FAILURE null)))))
                 (list ($foo 1)))

    (test-case "Show exceptions in log"
      (define (try makes-error)
        (check-match (get-subprogram-log (mdo x := (makes-error ((exc exn:fail:xiden) "blah ~a" 'abc))
                                              (subprogram (λ (m) (fail "Should not get here") (values FAILURE null)))))
                     (list ($show-string (pregexp "blah abc.+")))))

      (test-case "Show caught exceptions"
        (try (λ (e) (subprogram (λ (m) (raise e))))))))

  (test-case "Cycle detection"
    (define cyclic (subprogram-acyclic 1 (λ (m) (run-subprogram cyclic m))))
    (define acyclic (subprogram-acyclic 1 (λ (m) (values 'ok (cons 'ok m)))))

    (test-subprogram-procedure "Make acyclic procedures behave like normal subprogram procedures"
                               acyclic
                               (λ (v msg)
                                 (check-eq? v 'ok)
                                 (check-equal? msg '(ok))))

    (test-subprogram-procedure "Block cycles in acyclic subprogram procedures"
                               cyclic
                               (λ (v msg)
                                 (check-eq? v FAILURE)
                                 (check-equal? msg (list ($cycle 1)))))))