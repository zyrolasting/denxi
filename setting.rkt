#lang racket/base

; Define a setting as a single dynamically-bound Racket value derived
; from several configuration sources. Protect each setting with a
; contract.

(require (for-syntax racket/base
                     syntax/parse)
         "contract.rkt")

(provide (struct-out setting)
         define-setting
         define+provide-setting
         (contract-out
          [call-with-applied-settings
           (-> (if/c hash?
                     (hash/c setting? any/c)
                     (listof (cons/c setting? any/c)))
               (-> any)
               any)]))

(struct setting (id valid? parameter derived-parameter)
  #:property prop:procedure
  (case-lambda [(self) ((setting-derived-parameter self))]
               [(self v) ((setting-derived-parameter self) v)]
               [(self v proc) (parameterize ([(setting-derived-parameter self) v]) (proc))]))

(define (make-setting id-sym get-default valid?)
  (define param (make-parameter (void)))
  (define derived-parameter
    (make-derived-parameter
     param
     (make-setting-parameter-guard id-sym valid?)
     (make-setting-parameter-wrapper id-sym valid? get-default)))
  (setting id-sym
           valid?
           param
           derived-parameter))


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
    [(_ name:id cnt:expr get-default:expr)
     #'(define name (make-setting 'name (envvar-ref 'name get-default) cnt))]))


(define-syntax-rule (define+provide-setting id cnt get-default)
  (begin (provide (contract-out [id setting?]))
         (define-setting id cnt get-default)))


(define (envvar-ref envname default)
  (define env (getenv (symbol->string envname)))
  (cond [(not env) default]
        [(string=? env "") default]
        [else (read (open-input-string env))]))


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
    (define-setting TAKES_LIST (list/c real? string?) '(1 "foo"))
    (check-eq? (setting-id TAKES_LIST) 'TAKES_LIST)
    (check-pred (setting-valid? TAKES_LIST) '(1 "foo"))
    (check-pred (negate (setting-valid? TAKES_LIST)) '("foo" 1))
    (check-pred parameter? (setting-parameter TAKES_LIST))
    (check-pred parameter? (setting-derived-parameter TAKES_LIST))

    (test-exn "Reject invalid values"
              exn:fail:contract?
              (λ () (TAKES_LIST #f TAKES_LIST)))

    ; <!>
    (test-case "Parameterize new values"
      (check-equal? (TAKES_LIST '(2 "bar") TAKES_LIST) '(2 "bar"))
      (test-equal? "Old values are restored" (TAKES_LIST) '(1 "foo"))))

  (test-case "Define fallback values"
    (test-case "Compute fallback values using procedures"
      (define-setting NUM real? (const 1))
      ; (void) represents an unset setting
      (check-equal? (NUM (void) NUM) 1))
    (test-case "Use exact fallback values"
      (define-setting NUM real? 1)
      (check-equal? (NUM (void) NUM) 1))

    (test-exn "Validate computed fallback values"
              exn:fail:contract?
              (λ ()
                (define-setting NUM real? (const "not a number"))
                (NUM (void) NUM)))

    (test-exn "Validate exact fallback values"
              exn:fail:contract?
              (λ ()
                (define-setting NUM real? "not a number")
                (NUM (void) NUM))))

  (define-setting GROUP_A boolean? #f)
  (define-setting GROUP_B boolean? #f)

  ; This case is important because a command line handler can specify no flags.
  (test-equal? "Allow useless parameterizations"
               (call-with-applied-settings null
                                           (λ () 1))
               1))
