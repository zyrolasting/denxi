#lang racket/base

(provide assert test run-tests)

(require racket/function
         (for-syntax racket/base))


(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax ([src (syntax-source stx)] [line (syntax-line stx)])
       #'(or expr (halt src line 'expr)))]))


(define (halt src line v)
  (raise (exn:fail:user (format "~a:~a: ~s" src line v)
                        (current-continuation-marks))))


(define (run-tests module-path)
  (define module-path*
    `(submod ,module-path test))
  (define-values (provided _)
    (module->exports module-path*))
  (for* ([provisions (in-list provided)]
         [has-identifier (in-list (cdr provisions))])
    (define bound (dynamic-require module-path* (car has-identifier)))
    (when (and (procedure? bound)
               (procedure-arity-includes? bound 0))
      (bound))))


(define-syntax-rule (test id . xs)
  (begin (provide id)
         (define id (compose void (thunk . xs)))))
