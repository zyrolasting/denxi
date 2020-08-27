#lang racket/base

; Define $message, the root of a prefab structure type tree. Prefab
; structures are flexible enough to accomodate all of the following
; use cases at once:
;
;  - Visibility into data when something breaks
;  - Transmit data over place channels or ports
;  - Return values for monadic operations
;  - Compose data in custom shapes
;
; Also: Define imperative and functional approaches for managing
; $message instances. The functional approach uses monadic operations
; in the name of nicer tests and declarative code. The imperative
; approach is for things like reporting download progress.
;
; This module presumes a functional style, but allows you to switch to
; an imperative style when it makes sense.


(require "contract.rkt")

(provide (struct-out $message)
         accumulate-messages
         define-message
         define+provide-message
         imperative-style
         with-message-emitter
         (contract-out
          [attach-message
           (-> any/c $message? $with-messages?)]
          [call-with-message-emitter
           (-> (-> $message? any) (-> any) any)]
          [$with-messages/c
           (-> contract? contract?)]
          [emit-message!
           (-> (or/c $with-messages? $message? (listof $message?)) void?)]
          [current-output-emitter
           (parameter/c (or/c #f (-> $message? any)))]
          [:merge
           (-> $with-messages? $with-messages? $with-messages?)]
          [:do
           (->* () (#:with any/c) #:rest (listof procedure?)
                $with-messages?)]
          [:return
           (->* () (any/c) $with-messages?)]))


(struct $message () #:prefab)


(define-syntax define-message
  (syntax-rules ()
    [(_ id super-id (fields ...))
     (struct id super-id (fields ...) #:prefab)]
    [(_ id (fields ...))
     (define-message id $message (fields ...))]))


(define-syntax-rule (define+provide-message id rem ...)
  (begin (provide (struct-out id))
         (define-message id rem ...)))


(define+provide-message $with-messages (intermediate accumulated))


(define ($with-messages/c intermediate/c)
  (struct/c $with-messages intermediate/c (listof $message?)))


(define current-output-emitter
  (make-parameter void))


(define (:return [variant #f])
  ($with-messages variant null))


(define (:lift f)
  (compose :return f))


(define (:bind f)
  (λ (accum)
    (:merge accum
            (f ($with-messages-intermediate accum)))))


(define (:merge a b)
  ($with-messages ($with-messages-intermediate b)
                  (append ($with-messages-accumulated a)
                          ($with-messages-accumulated b))))


(define (:do #:with [v (:return (void))] . fs)
  (if (null? fs) v
      (with-handlers ([$with-messages? values])
        (apply :do #:with ((:bind (car fs)) v)
               (cdr fs)))))


(define (attach-message value message)
  ($with-messages value (list message)))


(define-syntax-rule (imperative-style body ...)
  (let ([ae (make-output-accumulating-emitter)])
    (call-with-message-emitter ae (λ () body ... (ae)))))


(define (call-with-message-emitter emit proc)
  (define old (current-output-emitter))
  (parameterize ([current-output-emitter
                  (λ (m) (emit m) (old m))])
    (proc)))


(define-syntax-rule (with-message-emitter emit body ...)
  (call-with-message-emitter emit (λ () body ...)))


(define-syntax-rule (accumulate-messages body ...)
  (let ([ae (make-message-accumulating-emitter)])
    (call-with-message-emitter ae (λ () body ... (ae)))))


(define (make-message-accumulating-emitter)
  (define o null)
  (case-lambda [() (reverse o)]
               [(m) (set! o (cons m o))]))


(define (make-output-accumulating-emitter)
  (define o (:return))
  (case-lambda [() o]
               [(m) (set! o (:merge o m))]))


(define (emit-message! msg)
  (cond [($message? msg)
         ((current-output-emitter) msg)]
        [($with-messages? msg)
         (emit-message! ($with-messages-accumulated msg))]
        [(list? msg)
         (for ([m (in-list msg)])
           (emit-message! m))]))


(module+ test
  (require racket/function
           rackunit)

  (define-message $derived (a b c))
  (define-message $ext $derived (d))
  (define-message $dummy (v))

  (test-pred "Extend $message by default" $message? ($derived 1 2 3))
  (test-pred "Extend other message if desired" $derived? ($ext 1 2 3 4))

  (test-pred "Recognize message attachments with a contract"
             ($with-messages/c real?)
             ($with-messages 10 (list ($dummy 1))))

  (test-equal? "Lift output procedure"
               ((:lift add1) 1)
               ($with-messages 2 null))

  (test-equal? "Normalize Racket values to $with-messages"
               (:return)
               ($with-messages #f null))

  (test-equal? "Bind output procedure"
               ((:bind (λ (v) (attach-message (add1 v) 'cool))) (:return 1))
               ($with-messages 2 '(cool)))

  (test-equal? "Compose output procedures"
               ((compose (:bind (λ (v) (attach-message (add1 v) '+)))
                         (:bind (λ (v) (attach-message (sub1 v) '-))))
                (:return 0))
               ($with-messages 0 '(- +)))

  (test-equal? "Accumulate messages in imperative style"
               (accumulate-messages
                 (emit-message! ($dummy 1))
                 (emit-message! ($dummy 2))
                 (emit-message! ($dummy 3)))
               (list ($dummy 1)
                     ($dummy 2)
                     ($dummy 3)))

  (test-equal? "Chain emitters"
               (accumulate-messages
                (emit-message! ($dummy 1))
                (check-equal? (accumulate-messages (emit-message! ($dummy 2)))
                              (list ($dummy 2)))
                (emit-message! ($dummy 3)))
               (list ($dummy 1)
                     ($dummy 2)
                     ($dummy 3)))

  (test-equal? "Switch to imperative style"
               (imperative-style
                (emit-message! (attach-message 1 ($dummy 1)))
                (emit-message! (attach-message 2 ($dummy 2)))
                (emit-message! (attach-message 'done ($dummy 3))))
               ($with-messages 'done
                               (list ($dummy 1)
                                     ($dummy 2)
                                     ($dummy 3))))

  (test-case "Make function sequence appear imperative"
    (define (add v) (attach-message (add1 v) '(add 1)))
    (define (sub v) (attach-message (- v 3)  '(sub 3)))
    (define (dbl v) (attach-message (* 2 v)  '(mul 2)))
    (define (sqr v) (attach-message (* v v)  `(mul ,v)))

    (define out (:do #:with (:return 5) add sub dbl sqr))
    (check-pred $with-messages? out)
    (check-equal? out
                  ($with-messages 36 '((add 1) (sub 3) (mul 2) (mul 6))))

    (test-equal? "Allow empty function sequence"
                 (:do #:with (:return))
                 (:return)))

  (test-equal? "Accumulate output from nested folds"
               (:do #:with ($with-messages #f '(hello))
                    (λ (m)
                      (:do (const (attach-message #f 'how))
                           (const (attach-message #f 'are))
                           (const (attach-message #f 'you)))))
               ($with-messages #f '(hello how are you))))
