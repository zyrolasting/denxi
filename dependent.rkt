#lang racket/base

(require racket/set
         "machine.rkt"
         "message.rkt"
         "monad.rkt")


(provide (struct-out $cycle)
         (struct-out dependent))


(define-message $cycle
  (dependency dependents))


(struct dependent (name dependencies install)
  #:property prop:procedure
  (λ (dep)
    (machine
     (λ (state)
       (define name (dependent-name dep))
       (define install (dependent-install dep))
       (define dependencies (dependent-dependencies dep))
       (define trace (current-trace))
       (define seen (car trace))
       (define dag-path (cdr trace))
       (if (set-member? seen name)
           (state-halt-with state ($cycle name dag-path))
           (with-continuation-mark mark-key
             (cons (set-add seen name)
                   (cons name dag-path))
             (mdo (machine-series dependencies)
                  (install (cdr (current-trace)))
                  (machine-unit (void)))))))))


(define (current-trace)
  (continuation-mark-set-first (current-continuation-marks)
                               mark-key
                               (list (set))))


(define mark-key
  (string->uninterned-symbol "denxi:cycle-detection"))
