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
                [(_ e:expr) #'e]
                [(_ target:id := e:expr . body)
                 #'(bind e (λ (target) (mdo . body)))]
                [(_ e:expr . body)
                 #'(bind e (λ _ (mdo . body)))]))


(module+ test
  (require racket/function
           rackunit)

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

  (test-pred "Adopts value of monadic type using mdo"
             include-string?
             program)

  (test-case "Compose operations with mdo"
    (call-with-values (λ () ((include-string-proc program) "start"))
                      (λ (v str)
                        (check-equal? v 4)
                        (check-equal? str "start+*")))))
