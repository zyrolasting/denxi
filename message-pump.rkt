#lang racket/base

; Define a means to process async messages.

(provide (all-defined-out)
         (all-from-out racket/contract))

(require racket/contract
         racket/place
         "logging.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/stx))

; Make prefab for use on place channels.
(struct $message () #:prefab)

(define-syntax-rule (define-message id (fields ...))
  (struct id $message (fields ...) #:prefab))

(define (default-message-handler s m)
  (error 'default-message-handler
         "Unknown message: ~s"
         m))

(define-syntax (define-message-pump stx)
  (syntax-case stx ()
    [(_ [id cnt default-handler] handler ...)
     (with-syntax ([(predicate ...) (stx-map (λ (s) (format-id stx "$~a?" s)) #'(handler ...))])
       #'(define/contract id
           (-> cnt $message? cnt)
           (let ([handlers (list (cons predicate handler)
                                 ...
                                 (cons (λ _ #t) default-handler))])
             (λ (state message)
               (apply (ormap (λ (pair)
                               (and ((car pair) message)
                                    (cdr pair)))
                             handlers)
                      state
                      (cdr (vector->list (struct->vector message))))))))]))


(module+ test
  (require rackunit)
  (struct example-state (v))
  (define-message $mset (v))
  (define-message $mmul (v))
  (define state (example-state #f))

  (define (mset s v)
    (example-state v))

  (define (mmul s v)
    (example-state (* (example-state-v s)
                      v)))

  (define-message-pump (foo example-state? default-message-handler)
    mset
    mmul)

  (test-exn "Require an instance of a state object"
            exn:fail:contract?
            (λ () (foo #f ($mset 1))))

  (test-exn "Requires an instance of a message"
            exn:fail:contract?
            (λ () (foo state #f)))

  (test-true "Functionally update state with a message"
             (and (eq? (example-state-v (foo state ($mset 1))) 1)
                  (eq? (example-state-v state) #f)))

  (test-eq? "Chain messages together"
             (example-state-v (foo (foo state ($mset 2))
                                   ($mmul 10)))
             20))
