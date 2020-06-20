#lang racket/base

; Provides a finite state machine that you can use to track the life
; cycle of a package.

(provide pkg-fsm
         assert-possible-transition)

(define rules
  '((uninstalled installing)

    (installing installed error)
    (installed updating uninstalling)

    (updating installed error)

    (uninstalling uninstalled error)

    (error uninstalled)))

(define (assert-possible-transition from to)
  ((pkg-fsm from) to))

(define (find-allowed state)
  (define rule (findf (λ (rule) (eq? (car rule) state)) rules))
  (if rule (cdr rule) null))

(define (pkg-fsm [state 'uninstalled])
  (let ([allowed (find-allowed state)])
    (procedure-rename
     (case-lambda
       [() state]
       [(next-state)
        (if (or (eq? state next-state)
                (member next-state allowed))
            (pkg-fsm next-state)
            (error 'pkg-fsm
                   "Cannot change package state from ~v to ~v. expected: ~v"
                   state
                   next-state
                   allowed))])
     state)))

(module+ test
  (require rackunit)
  (define (lifecycle steps)
    (for/fold ([fsm (pkg-fsm)])
              ([next (in-list steps)])
      (fsm next)))

  (test-not-exn "Package lifecycle"
                (λ ()
                  (lifecycle '(installing
                               installed
                               updating
                               installed
                               uninstalling
                               uninstalled))))

  (test-exn "A package cannot jump to being installed"
            exn:fail?
            (λ () (lifecycle '(installed)))))
