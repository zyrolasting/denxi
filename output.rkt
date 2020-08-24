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
 (contract-out
  [make-$with-output
   (-> any/c any/c (listof $message?) $with-output?)]

  [current-output-emitter
   (parameter/c (or/c #f (-> $message? any)))]

  [:stopped?
   (-> $with-output? boolean?)]

  [:done
   (->* ((or/c $message? (listof $message?)))
        $with-output?)]

  [:fail
   (->* ((or/c $message? (listof $message?)))
        (#:stop-value exact-positive-integer?)
        $with-output?)]

  [:fold
   (-> any/c
       (non-empty-listof procedure?)
       $with-output?)]

  [:return
   (->* ()
        (any/c
         (or/c $message? (listof $message?))
         #:stop-value any/c)
        any/c)]

  [:unit
   (->* () (any/c) $with-output?)]

  [:bind
   (-> (-> any/c $with-output?)
       (-> $with-output? $with-output?))]

  [:lift
   (-> (-> any/c any/c)
       (-> any/c $with-output?))]))


(define current-output-emitter
  (make-parameter #f))


(define (make-$with-output stop-value intermediate messages)
  ; Sometimes sending output as a side-effect is desirable.
  ; The main use case is reporting progress on long running jobs.
  (let ([emit! (current-output-emitter)])
    (unless (or (null? messages) (not emit!))
      (for ([m (in-list messages)]) (emit! m))))
  ($with-output stop-value intermediate messages))


(define (:done [m null])
  (:return #:stop-value 0 #f m))


(define (:stopped? m)
  (and ($with-output-stop-value m)
       #t))


(define (:fail #:stop-value [stop-value 1] [m null])
  (:return #:stop-value stop-value #f m))


(define (:fold initial fs)
  ((apply compose (map :bind (reverse fs)))
   (if ($with-output? initial)
       initial
       (:unit initial))))


(define (:unit [v #f])
  (make-$with-output #f v null))


(define (:lift f)
  (procedure-rename
   (compose :unit f)
   (string->symbol (format "~a/with-output/lifted"
                           (object-name f)))))


(define (:return #:stop-value [stop-value #f] [val #f] [msg null])
  (make-$with-output stop-value
                     val
                     (if (list? msg) msg (list msg))))



(define (:bind f)
  (procedure-rename
   (λ (accum)
     (if (:stopped? accum)
         accum
         (with-handlers
           ([$message?
             (λ (m) (:return #:stop-value 1
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
  (require racket/function
           rackunit)

  (test-equal? "Lift output procedure"
               ((:lift add1) 1)
               ($with-output #f 2 null))

  (test-equal? "Abbreviate :return for success"
               (:done 1)
               (:return #:stop-value 0 #f 1))

  (test-equal? "Abbreviate :return for general failure"
               (:fail 'a)
               (:return #:stop-value 1 #f 'a))

  (test-equal? "Abbreviate :return for specific failure"
               (:fail 'a #:stop-value 2)
               (:return #:stop-value 2 #f 'a))

  (test-case "Compose output-producing functions"
    (define (add v) (:return (add1 v) '(add 1)))
    (define (sub v) (:return (- v 3)  '(sub 3)))
    (define (dbl v) (:return (* 2 v)  '(mul 2)))
    (define (sqr v) (:return (* v v)  `(mul ,v)))

    (define out (:fold 5 (list add sub dbl sqr)))
    (check-pred $with-output? out)
    (check-equal? out
                  ($with-output #f 36 '(add 1 sub 3 mul 2 mul 6))))

  (test-case "Dynamically control output as a side-effect"
    (define imperatively-collected null)
    (define (meta v) (:bind (λ (_) (:return v (list v)))))
    (define-values (a b c d e)
      (apply values (map meta '(a b c d e))))

    (define (instrument f)
      (λ (v)
        (parameterize ([current-output-emitter
                        (λ (m) (set! imperatively-collected (cons m imperatively-collected)))])
          (f v))))

    (define functionally-collected
      ((compose e (instrument d) (instrument c) b a)
       (:unit)))

    (check-equal? ($with-output-accumulated functionally-collected) '(a b c d e))
    (check-equal? imperatively-collected '(d c)))

  (test-case "Stop output compositions with raised messages errors"
    (define e ($fail "uh oh"))
    (define (add v) (:return (add1 v) '(add 1)))
    (define (err v) (raise e))

    (check-equal? (:fold 5 (list add err add))
                  ($with-output 1 6 `(add 1 ,e))))

  (test-case "Stop output compositions with return value"
    (define (add v) (:return (add1 v) 1))
    (define (err v) (:return #:stop-value 'stop))

    (check-equal? (:fold 5 (list add err add))
                  ($with-output 'stop #f '(1))))

  (test-equal? "Accumulate output from nested folds"
               (:fold (:return #f 'hello)
                      (list
                       (λ (m)
                         (:fold (void)
                                (list (const (:return #f 'how))
                                      (const (:return #f 'are))
                                      (const (:return #f 'you)))))))
               ($with-output #f #f
                             '(hello how are you))))
