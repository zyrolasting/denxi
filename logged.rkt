#lang racket/base

; Define a monadic type

(require "contract.rkt")
(provide (struct-out logged)
         SUCCESS
         FAILURE
         Logged
         logged-unit
         logged-failure
         logged-attachment
         messy-log/c
         run-log
         get-log
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
         "monad.rkt")

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


(module+ test
  (require rackunit)

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
