#lang racket/base

; Define $message, the root of a prefab structure type tree. Prefab
; structures are flexible enough to accomodate all of the following
; use cases at once:
;
;  - Visibility into data when something breaks
;  - Transmit data over place channels or ports
;  - Return values for monadic operations
;  - Compose data in custom shapes

(require "contract.rkt")
(provide (struct-out $message)
         (struct-out logged)
         SUCCESS
         FAILURE
         define-message
         define+provide-message
         Logged
         logged-unit
         logged-failure
         run-log
         get-log)

(require racket/list
         "exn.rkt"
         "format.rkt"
         "monad.rkt")

(define-syntax define-message
  (syntax-rules ()
    [(_ id super-id (fields ...))
     (struct id super-id (fields ...) #:prefab)]
    [(_ id (fields ...))
     (define-message id $message (fields ...))]))

(define-syntax-rule (define+provide-message id rem ...)
  (begin (provide (struct-out id))
         (define-message id rem ...)))


(struct $message () #:prefab)
(define+provide-message $show-datum  (value))
(define+provide-message $show-string (message))

(define SUCCESS (string->uninterned-symbol "SUCCESS"))
(define FAILURE (string->uninterned-symbol "FAILURE"))

(struct logged (thnk)
  #:transparent
  #:methods gen:monad [(define (monad->monad-class m) Logged)])

(define (logged-unit v)
  (logged (λ (m) (values v m))))

(define (logged-failure e)
  (logged
   (λ (m)
     (values FAILURE
             (cons (cond [(exn? e) ($show-string (exn->string e))]
                         [($message? e) e]
                         [else (~s e)])
                       m)))))

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


(define (get-log m)
  (define-values (_ messages) (run-log m))
  (reverse (flatten messages)))

(module+ test
  (require rackunit)

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
