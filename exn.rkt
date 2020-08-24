#lang racket/base

; Define exception hierarchy, plus some helper procedures to deal with
; the tedium of juggling fields and formatted messages.

(require racket/contract
         racket/exn)

(provide (all-from-out racket/exn)
         define-exn
         (contract-out
          [rex
           (->* ((unconstrained-domain-> exn?))
                #:rest list?
                any)]

          [exc (->* ((unconstrained-domain-> exn?))
                          #:rest list?
                          (unconstrained-domain-> exn?))]
          [make-xiden-error
           (->* ((unconstrained-domain-> exn?) string?)
                (#:fields null)
                #:rest list?
                exn?)]))

(define-syntax-rule (define-exn id super-id (fields ...))
  (begin (provide (struct-out id))
         (struct id super-id (fields ...) #:transparent)))

; This two step process binds exception structure fields, followed by
; the formatted message.
;
; e.g. ((exc exn:fail:xiden:source:cannot-find-size "field") "Formatted: ~a" 1)
;
; That is equivalent to
;
; (exn:fail:xiden:source:cannot-find-size
;  (format "Formatted: ~a" 1)
;  (current-continuation-marks)
;  "field")
;
(define (exc ctor . fields)
  (λ ([fmt-message ""] . fmt-args)
    (apply make-xiden-error #:fields fields ctor fmt-message fmt-args)))

(define (rex ctor . fields)
  (raise ((apply exc ctor fields))))

(define (make-xiden-error #:fields [fields null] ctor fmt-message . fmt-args)
  (apply ctor
         (apply format fmt-message fmt-args)
         (current-continuation-marks)
         fields))



(define-exn exn:fail:xiden exn:fail ())
(define-exn exn:fail:xiden:decompression exn:fail:xiden (pos))
(define-exn exn:fail:xiden:invalid-revision-interval exn:fail:xiden (lo hi))
(define-exn exn:fail:xiden:invalid-racket-version-interval exn:fail:xiden (lo hi))

(define-exn exn:fail:user:xiden exn:fail:user ())
(define-exn exn:fail:user:xiden:config exn:fail:xiden ())
(define-exn exn:fail:user:xiden:config:unreadable-setting exn:fail:user:xiden:config ())
(define-exn exn:fail:user:xiden:config:invalid-setting-value exn:fail:user:xiden:config ())

(define-exn exn:fail:xiden:source exn:fail:xiden (input-name source))
(define-exn exn:fail:xiden:source:no-content exn:fail:xiden:source ())
(define-exn exn:fail:xiden:source:digest-mismatch exn:fail:xiden:source ())
(define-exn exn:fail:xiden:source:signature-mismatch exn:fail:xiden:source ())

(define-exn exn:fail:xiden:openssl exn:fail:xiden (exit-code))


(module+ test
  (require rackunit)

  (define-exn exn:xiden:test exn (a))
  (define-exn exn:xiden:test:extra exn:xiden:test (b c))

  (define (check-instance e)
    (check-pred exn? e)
    (check-pred exn:xiden:test? e)
    (check-pred exn:xiden:test:extra? e)
    (check-pred continuation-mark-set? (exn-continuation-marks e))
    (check-equal? (exn-message e) "Easy as Do, Re, Mi, A, B, C.")
    (check-eq? (exn:xiden:test-a e) 1)
    (check-eq? (exn:xiden:test:extra-b e) 2)
    (check-eq? (exn:xiden:test:extra-c e) 3))

  (define fmt-args
    '("Easy as ~a, ~a, ~a, ~a, ~a, ~a."
      "Do" "Re" "Mi" A B C))

  (test-case "Create exceptions using a one-step process"
    (check-instance
     (apply make-xiden-error exn:xiden:test:extra #:fields '(1 2 3) fmt-args))
    (test-not-exn "Do not require field declarations"
                  (λ () (make-xiden-error exn:fail:xiden "")))

    (test-exn "Raise exceptions with fields only"
              (struct/c exn:xiden:test:extra string? continuation-mark-set? 1 2 3)
              (λ () (rex exn:xiden:test:extra 1 2 3))))

  (test-case "Create exceptions using a two-step process"
    (check-instance (apply (exc exn:xiden:test:extra 1 2 3) fmt-args))
    (test-not-exn "Do not require messages"
                  (λ () ((exc exn:xiden:test:extra 1 2 3))))))
