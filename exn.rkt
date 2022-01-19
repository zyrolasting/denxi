#lang s-exp "base.rkt"

(reprovide racket/exn)

(provide pitch
         (contract-out
          [dynamic-pitch
           (->* (#:catcher
                 (if/c procedure?
                       (or/c (procedure-arity-includes/c 0)
                             (procedure-arity-includes/c 1))
                       any/c)
                 #:batter
                 (-> any))
                (#:umpire procedure?)
                any)]))
                

(require racket/function
         (for-syntax racket/base))

(module+ test
  (require rackunit)
  (test-case "Catch exceptions"
    (check-equal? (pitch 1 (error)) 1)
    (check-pred exn? (pitch values (error)))
    (check-equal? (pitch (λ () 1) (error)) 1)
    (check-true (pitch #t (pitch (λ (a b) 1) (error) #f)))))

(define-syntax-rule (pitch c . xs)
  (dynamic-pitch #:catcher c #:batter (λ () . xs)))

; Read as analogy of a pitch in baseball
(define (dynamic-pitch #:umpire [dispute values] #:catcher catch #:batter swing)
  (let/ec decide
    (define return
      (compose decide dispute))

    (define (exception-handler e)
      (return (if (procedure? catch)
                  (if (procedure-arity-includes? catch 1)
                      (catch e)
                      (if (procedure-arity-includes? catch 0)
                          (catch)
                          (raise e)))
                  catch)))
    
    (with-handlers ([(negate exn:break?) exception-handler])
      (if (eq? dispute values)
          (swing)
          (call-with-values swing return)))))
