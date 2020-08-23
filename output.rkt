#lang racket/base

; Define monadic operations to accumulate output.  Emitting output
; elsewhere as a side-effect is allowed in the abstract, but the
; nature of those side-effects must be specified.
;
; This is all in the name of making better tests and carefully
; controlling side-effects in long-running jobs.

(require "contract.rkt"
         "message.rkt"
         "xiden-messages.rkt")

(provide
 :>
 (rename-out [output-return  :use]
             [output-failure :fail]
             [output-success :done])
 (contract-out
  [make-$with-output
   (-> any/c any/c (listof $message?) $with-output?)]
  [current-output-emitter
   (parameter/c (or/c #f (-> $message? any)))]
  [output-success
   (->* ((or/c $message? (listof $message?)))
        $with-output?)]
  [output-failure
   (->* ((or/c $message? (listof $message?)))
        (#:stop-value exact-positive-integer?)
        $with-output?)]
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
   (->* () (any/c) $with-output?)]
  [output-bind
   (-> (-> any/c $with-output?)
       (-> $with-output? $with-output?))]
  [output-lift
   (-> (-> any/c any/c)
       (-> any/c $with-output?))]))


(define-syntax-rule (:> initial [sig body ...] ...)
  (output-fold initial
               (list (λ sig body ...)
                     ...)))

(define current-output-emitter
  (make-parameter #f))

(define (make-$with-output stop-value intermediate messages)
  ; Sometimes sending output as a side-effect is desirable.
  ; The main use case is reporting progress on long running jobs.
  (let ([emit! (current-output-emitter)])
    (unless (or (null? messages) (not emit!))
      (for ([m (in-list messages)]) (emit! m))))
  ($with-output stop-value intermediate messages))

(define (output-success [m null])
  (output-return #:stop-value 0 #f m))


(define (output-failure #:stop-value [stop-value 1] [m null])
  (output-return #:stop-value stop-value #f m))


(define (output-fold initial fs)
  ((apply compose (map output-bind (reverse fs)))
   (output-unit initial)))


(define (output-unit [v #f])
  (make-$with-output #f v null))


(define (output-lift f)
  (procedure-rename
   (compose output-unit f)
   (string->symbol (format "~a/with-output/lifted"
                           (object-name f)))))


(define (output-return #:stop-value [stop-value #f] [val #f] [msg null])
  (make-$with-output stop-value
                     val
                     (if (list? msg) msg (list msg))))


(define (output-bind f)
  (procedure-rename
   (λ (accum)
     (if ($with-output-stop-value accum)
         accum
         (with-handlers
           ([$message?
             (λ (m) (output-return #:stop-value 1
                                   ($with-output-intermediate accum)
                                   (append ($with-output-accumulated accum)
                                           (list m))))])
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

  (test-equal? "Abbreviate output-return for success"
               (output-success 1)
               (output-return #:stop-value 0 #f 1))

  (test-equal? "Abbreviate output-return for general failure"
               (output-failure 'a)
               (output-return #:stop-value 1 #f 'a))

  (test-equal? "Abbreviate output-return for specific failure"
               (output-failure 'a #:stop-value 2)
               (output-return #:stop-value 2 #f 'a))

  (test-case "Compose output-producing functions"
    (define (add v) (output-return (add1 v) '(add 1)))
    (define (sub v) (output-return (- v 3)  '(sub 3)))
    (define (dbl v) (output-return (* 2 v)  '(mul 2)))
    (define (sqr v) (output-return (* v v)  `(mul ,v)))

    (define out (output-fold 5 (list add sub dbl sqr)))
    (check-pred $with-output? out)
    (check-equal? out
                  ($with-output #f 36 '(add 1 sub 3 mul 2 mul 6))))

  (test-case "Dynamically control output as a side-effect"
    (define imperatively-collected null)
    (define (meta v) (output-bind (λ (_) (output-return v (list v)))))
    (define-values (a b c d e)
      (apply values (map meta '(a b c d e))))

    (define (instrument f)
      (λ (v)
        (parameterize ([current-output-emitter
                        (λ (m) (set! imperatively-collected (cons m imperatively-collected)))])
          (f v))))

    (define functionally-collected
      ((compose e (instrument d) (instrument c) b a)
       (output-unit)))

    (check-equal? ($with-output-accumulated functionally-collected) '(a b c d e))
    (check-equal? imperatively-collected '(d c)))

  (test-case "Stop output compositions with raised messages errors"
    (define e ($fail "uh oh"))
    (define (add v) (output-return (add1 v) '(add 1)))
    (define (err v) (raise e))

    (check-equal? (output-fold 5 (list add err add))
                  ($with-output 1 6 `(add 1 ,e))))

  (test-case "Stop output compositions with return value"
    (define (add v) (output-return (add1 v) 1))
    (define (err v) (output-return #:stop-value 'stop))

    (check-equal? (output-fold 5 (list add err add))
                  ($with-output 'stop #f '(1)))))
