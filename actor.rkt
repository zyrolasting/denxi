#lang racket/base

; Define an object that integrates messages with methods.

(provide actor%
         (all-from-out racket/class))

(require racket/class
         racket/string
         "message.rkt")

(define (destructure-message message)
  (let ([message (vector->list (struct->vector message))])
    (values (car message)
            (cdr message))))

(define (message-id->method-id message-id)
  (string->symbol
   (string-replace
    (symbol->string message-id)
    "struct:" "handle-")))

(define actor%
  (class object%
    (define/public (pump message)
      (let-values ([(message-id args) (destructure-message message)])
        (apply dynamic-send this (message-id->method-id message-id) args)))
    (super-new)))


(module+ test
  (require racket/file
           racket/set
           rackunit)

  (define-message $derived (a b c))

  (test-case "Destructure a $message"
    (define msg ($derived 1 2 3))
    (define-values (key args) (destructure-message msg))
    (check-eq? key 'struct:$derived)
    (check-equal? args '(1 2 3))

    (test-equal? "Convert symbol for message id to method id"
                 (message-id->method-id key)
                 'handle-$derived)

    (test-case "Call actor% methods with messages"
      (define got #f)
      (define dumb-actor%
        (class actor%
          (define/public (handle-$derived . args)
            (test-equal? "Reconstruct message in method" (apply $derived args) msg)
            (set! got #t))
          (super-new)))

      (define a (new dumb-actor%))
      (send a pump msg)
      (test-true "Call handler from message" got))))
