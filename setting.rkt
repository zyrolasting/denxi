#lang racket/base

; Define a setting as a single dynamically-bound Racket value derived
; from several configuration sources. Protect each setting with a
; contract.

(require "contract.rkt")
(provide (struct-out setting)
         define-setting
         define-setting-group
         (contract-out
          [call-with-applied-settings
           (-> (if/c hash?
                     (hash/c setting? any/c)
                     (listof (cons/c setting? any/c)))
               (-> any)
               any)]))


(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         "string.rkt"
         "url.rkt")


(struct setting (id valid? parameter derived-parameter description)
  #:property prop:procedure
  (case-lambda [(self) ((setting-derived-parameter self))]
               [(self v proc) (parameterize ([(setting-derived-parameter self) v]) (proc))]))

(define (make-setting id-sym get-default valid? desc)
  (define param (make-parameter (void)))
  (define derived-parameter
    (make-derived-parameter
     param
     (make-setting-parameter-guard id-sym valid?)
     (make-setting-parameter-wrapper id-sym valid? get-default)))
  (setting id-sym
           valid?
           param
           derived-parameter
           desc))


(define (make-setting-parameter-guard id valid?)
  (λ (proposed-value)
    (with-handlers ([exn:fail:contract?
                     (λ (e) (raise (rewrite-contract-error-message e id)))])
      (invariant-assertion (or/c void? valid?) proposed-value))))


(define (make-setting-parameter-wrapper id valid? get-default)
  (λ (v) (if (void? v)
             (with-handlers ([exn:fail:contract?
                              (λ (e) (raise (rewrite-contract-error-message e id)))])
               (invariant-assertion valid?
                                    (if (procedure? get-default)
                                        (get-default id)
                                        get-default)))
             v)))


; Calls proc in a parameterization where an alist or hash
; determines a new value for each setting.
(define (call-with-applied-settings variant proc)
  (define h
    (if (list? variant)
        (make-immutable-hash variant)
        variant))

  ((for/fold ([wip proc])
             ([(s v) (in-hash h)])
     (λ ()
       (parameterize ([(setting-derived-parameter s) v])
         (wip))))))


(define-syntax (define-setting stx)
  (syntax-parse stx
    [(_ name:id cnt:expr get-default:expr description:expr)
     #'(define name (make-setting 'name get-default cnt description))]))


(define-syntax-rule (define-setting-group id (s ...))
  (define id (make-immutable-hash (list (cons (setting-id s) s) ...))))


(module+ test
  (require racket/function
           racket/cmdline
           racket/match
           rackunit)

  ; Below you'll see a pattern of the form (SETTING <value> SETTING).
  ; (SETTING <value> <proc>) means "Call <proc> with SETTING bound to
  ; <value>". Since (SETTING) returns the current value of the setting
  ; for the current parameterization, (SETTING <value> SETTING) is a
  ; way to rebind SETTING, return the set value, then restore the
  ; original value in a single expression. See the test marked with
  ; <!> for an example of this.

  (test-case "Define a setting"
    (define help-strings "Desc")
    (define-setting TAKES_LIST (list/c real? string?) '(1 "foo") help-strings)
    (check-eq? (setting-id TAKES_LIST) 'TAKES_LIST)
    (check-pred (setting-valid? TAKES_LIST) '(1 "foo"))
    (check-pred (negate (setting-valid? TAKES_LIST)) '("foo" 1))
    (check-pred parameter? (setting-parameter TAKES_LIST))
    (check-pred parameter? (setting-derived-parameter TAKES_LIST))
    (check-equal? (setting-description TAKES_LIST) help-strings)

    (test-exn "Reject invalid values"
              exn:fail:contract?
              (λ () (TAKES_LIST #f TAKES_LIST)))

    ; <!>
    (test-case "Parameterize new values"
      (check-equal? (TAKES_LIST '(2 "bar") TAKES_LIST) '(2 "bar"))
      (test-equal? "Old values are restored" (TAKES_LIST) '(1 "foo"))))

  (test-case "Define fallback values"
    (test-case "Compute fallback values using procedures"
      (define-setting NUM real? (const 1) "number!")
      ; (void) represents an unset setting
      (check-equal? (NUM (void) NUM) 1))
    (test-case "Use exact fallback values"
      (define-setting NUM real? 1 "number!")
      (check-equal? (NUM (void) NUM) 1))

    (test-exn "Validate computed fallback values"
              exn:fail:contract?
              (λ ()
                (define-setting NUM real? (const "not a number") "number!")
                (NUM (void) NUM)))

    (test-exn "Validate exact fallback values"
              exn:fail:contract?
              (λ ()
                (define-setting NUM real? "not a number" "number!")
                (NUM (void) NUM))))

  (define-setting GROUP_A boolean? #f "Fuh")
  (define-setting GROUP_B boolean? #f "Buh")
  (define-setting-group GROUP [GROUP_A GROUP_B])

  (test-true "Define settings in groups"
             (and (hash? GROUP)
                  (immutable? GROUP)
                  (eq? (hash-ref GROUP 'GROUP_A) GROUP_A)
                  (eq? (hash-ref GROUP 'GROUP_B) GROUP_B)))

  ; This case is important because a command line handler can specify no flags.
  (test-equal? "Allow useless parameterizations"
               (call-with-applied-settings null
                                           (λ () 1))
               1))
