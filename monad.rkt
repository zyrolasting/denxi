#lang racket/base

; Limited monad library with monomorphic bind and related do notation

(provide (all-defined-out))

(require (for-syntax racket/base
                     syntax/parse)
         racket/generic)

(define-generics monad
  [bind monad continue])

(define-syntax (:= stx)
  (raise-syntax-error #f ":= outside mcompose" stx))

(define-syntax (mdo stx)
  (syntax-parse stx #:literals (:=)
                [(_) #'(void)]
                [(_ e:expr) #'e]
                [(_ target:id := e:expr . body)
                 #'(bind e (λ (target) (mdo . body)))]
                [(_ e:expr . body)
                 #'(bind e (λ _ (mdo . body)))]))


(module+ test
  (require racket/function
           "test.rkt")

  (struct include-string (proc)
    #:methods gen:monad
    [(define (bind ma f) (include-string-bind ma f))])

  (define (include-string-bind ma f)
    (include-string
     (λ (str)
       (define-values (v str*) ((include-string-proc ma) str))
       (define-values (v* str**) ((include-string-proc (f v)) str*))
       (values v* str**))))

  (define (include-string-return v)
    (include-string (curry values v)))

  (define program
    (mdo a := (include-string-return 1)
         b := (include-string (λ (str) (values (add1 a) (string-append str "+"))))
         (include-string (λ (str) (values (* b 2) (string-append str "*"))))))

  (test empty-mdo
        (assert (void? (mdo))))

  (test monomorphism
        (assert (include-string? program)))

  (test composition
        (call-with-values (λ () ((include-string-proc program) "start"))
                          (λ (v str)
                            (assert (equal? v 4))
                            (assert (equal? str "start+*"))))))
