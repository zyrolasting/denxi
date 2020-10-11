#lang racket/base

; Define $message, the root of a prefab structure type tree. Prefab
; structures are flexible enough to accomodate all of the following
; use cases at once:
;
;  - Visibility into data when something breaks
;  - Transmit data over place channels or ports
;  - Return values for monadic operations
;  - Compose data in custom shapes

(provide (struct-out $message)
         define-message
         define+provide-message
         scope-message
         call-in-message-scope
         call-in-message-scope*
         in-message-scope
         get-message-scope)

(define-syntax define-message
  (syntax-rules ()
    [(_ id super-id (fields ...))
     (struct id super-id (fields ...) #:prefab)]
    [(_ id (fields ...))
     (define-message id $message (fields ...))]))

(define-syntax-rule (define+provide-message id rem ...)
  (begin (provide (struct-out id))
         (define-message id rem ...)))


(struct $message () #:prefab)
(define+provide-message $show-datum  (value))
(define+provide-message $show-string (message))
(define+provide-message $regarding   (subject body))


(define mark-key (string->uninterned-symbol "xiden:message-scope"))

(define (get-message-scope)
  (or (continuation-mark-set-first
       (current-continuation-marks)
       mark-key)
      null))

(define (scope-message m [scope (get-message-scope)])
  (if (null? scope) m
      (scope-message ($regarding (car scope) m)
                     (cdr scope))))

(define (call-in-message-scope* ms proc)
  (with-continuation-mark mark-key ms
    (proc)))

(define (call-in-message-scope m proc)
  (call-in-message-scope* (cons m (get-message-scope))
                          proc))

(define-syntax-rule (in-message-scope m body ...)
  (call-in-message-scope m (λ () body ...)))


(module+ test
  (require rackunit)

  (define-message $foo (a b c))
  (define foo-inst ($foo 1 2 3))

  (check-pred $foo? foo-inst)
  (check-equal? ($foo-a foo-inst) 1)
  (check-equal? ($foo-b foo-inst) 2)
  (check-equal? ($foo-c foo-inst) 3)

  (define-message $scope (v))
  (test-case "Scope messages"
    (in-message-scope ($scope 1)
       (in-message-scope ($scope 2)
         (check-equal? (get-message-scope)
                       (list ($scope 2)
                             ($scope 1)))
         (check-equal? (scope-message foo-inst)
                       ($regarding ($scope 1)
                                   ($regarding ($scope 2)
                                               foo-inst)))))))
