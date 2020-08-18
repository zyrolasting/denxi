#lang racket/base

; Define monadic operations to accumulate output.

(require "contract.rkt"
         "message.rkt"
         "xiden-messages.rkt")

(provide
 (contract-out
  [output-fold
   (-> any/c
       (non-empty-listof procedure?)
       $with-output?)]
  [output-return
   (->* ()
        (any/c
         (or/c $message? (listof $message?))
         #:stop-value any/c)
        any/c)]
  [output-unit
   (-> any/c $with-output?)]
  [output-bind
   (-> (-> any/c $with-output?)
       (-> $with-output? $with-output?))]
  [output-lift
   (-> (-> any/c any/c)
       (-> any/c $with-output?))]))


(define (output-fold initial fs)
  ((apply compose (map output-bind (reverse fs)))
   (output-unit initial)))


(define (output-unit v)
  ($with-output #f v null))


(define (output-lift f)
  (procedure-rename
   (compose output-unit f)
   (string->symbol (format "~a/with-output/lifted"
                           (object-name f)))))


(define (output-return #:stop-value [stop-value #f] [val #f] [msg null])
  ($with-output stop-value
                val
                (if (list? msg) msg (list msg))))


(define (output-bind f)
  (procedure-rename
   (λ (accum)
     (if ($with-output-stop-value accum)
         accum
         (with-handlers
           ([values
             (λ (e) (output-return #:stop-value e
                                   ($with-output-intermediate accum)
                                   ($with-output-accumulated accum)))])
           (let ([next (f ($with-output-intermediate accum))])
             ($with-output ($with-output-stop-value next)
                           ($with-output-intermediate next)
                           (append ($with-output-accumulated accum)
                                   ($with-output-accumulated next)))))))
   (string->symbol (format "~a/with-output/bound"
                           (object-name f)))))



(module+ test
  (require rackunit)

  (test-equal? "Lift output procedure"
               ((output-lift add1) 1)
               ($with-output #f 2 null))

  (test-case "Compose output-producing functions"
    (define (add v) (output-return (add1 v) '(add 1)))
    (define (sub v) (output-return (- v 3)  '(sub 3)))
    (define (dbl v) (output-return (* 2 v)  '(mul 2)))
    (define (sqr v) (output-return (* v v)  `(mul ,v)))

    (define out (output-fold 5 (list add sub dbl sqr)))
    (check-pred $with-output? out)
    (check-equal? out
                  ($with-output #f 36 '(add 1 sub 3 mul 2 mul 6))))

  (test-case "Stop output compositions with errors"
    (define e (exn:fail "uh oh" (current-continuation-marks)))
    (define (add v) (output-return (add1 v) '(add 1)))
    (define (err v) (raise e))

    (check-equal? (output-fold 5 (list add err add))
                  ($with-output e 6 '(add 1))))

  (test-case "Stop output compositions with return value"
    (define (add v) (output-return (add1 v) 1))
    (define (err v) (output-return #:stop-value 'stop))

    (check-equal? (output-fold 5 (list add err add))
                  ($with-output 'stop #f '(1)))))
