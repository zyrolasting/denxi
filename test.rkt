#lang racket/base

(provide assert
         match?
         test
         run-all-tests
         run-tests)

(require racket/function
         racket/match
         racket/runtime-path
         racket/path
         (for-syntax racket/base))


(define-runtime-path here ".")

(define failure (gensym))
(define failure? (curry eq? failure))


(struct dynamic-test (procedure)
  #:property prop:procedure
  (λ (self)
    (with-handlers ([failure? void])
      ((dynamic-test-procedure self)))))


(define-syntax-rule (match? value pattern)
  (match value [pattern #t] [_ #f]))


(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax ([src (syntax-source stx)] [line (syntax-line stx)])
       #'(dynamic-assert src line expr 'expr))]))


(define (dynamic-assert src line result expr-datum)
  (printf "~a (~a, line ~a): ~s~n"
          (if result "pass" "fail")
          (and src (file-name-from-path src))
          line
          expr-datum)
  (unless result (raise failure)))


(define (run-tests module-path)
  (define-values (provided _)
    (module->exports module-path))
  (for* ([provisions (in-list provided)]
         [has-identifier (in-list (cdr provisions))])
    (define bound (dynamic-require module-path (car has-identifier)))
    (when (dynamic-test? bound)
      (bound))))


(define (run-all-tests)
  (parameterize ([current-directory here])
    (for ([file-path (in-directory)]
          #:when (regexp-match? #px"\\.rkt$" file-path))
      (define module-path
        `(submod ,(format "~a" (find-relative-path (current-directory) file-path)) test))
      (define instantiation-status
        (with-handlers ([values values])
          (dynamic-require module-path #f)))
      (unless (exn? instantiation-status)
        (writeln module-path)
        (run-tests module-path)))))


(define-syntax-rule (test id . xs)
  (begin (provide id)
         (define id (dynamic-test (λ () . xs)))))
