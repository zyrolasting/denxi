#lang racket/base

; Define an object that integrates messages with methods.

(provide actor%
         (all-from-out racket/class))

(require racket/class
         "message.rkt")


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

  (define-message $m (v))

  (test-case "Call actor% methods with messages"
    (define got-$m #f)
    (define dumb-actor%
      (class actor%
        (define/public (handle-$m a)
          (test-eq? "Got $m value" a 1)
          (set! got-$m #t))
        (super-new)))

    (define a (new dumb-actor%))
    (send a pump ($m 1))
    (test-true "Call $m handler" got-$m)))
