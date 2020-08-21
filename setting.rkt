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
           (-> (or/c (hash/c setting? any/c)
                     (non-empty-listof (cons/c setting? any/c)))
               (-> any)
               any)]
          [settings->flag-specs
           (->* () #:rest (listof setting?) list?)]))


(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         "string.rkt"
         "url.rkt"
         "xiden-messages.rkt")


(struct setting (id valid? parameter derived-parameter short-flag long-flag help-strings)
  #:property prop:procedure
  (case-lambda [(self) ((setting-derived-parameter self))]
               [(self v proc) (parameterize ([(setting-derived-parameter self) v]) (proc))]))



(define (make-setting id-sym get-default valid? short-flag help-strings)
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
           short-flag
           (format "--~a" id-sym)
           help-strings))


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


(define (make-flag-spec s)
  (list (let ([long (setting-long-flag s)]
              [short (setting-short-flag s)])
          (if short
              (list short long)
              (list long)))
        (λ (flag user-asserted-value)
          (cons s (read (open-input-string user-asserted-value))))
        (setting-help-strings s)))


(define (call-with-applied-settings variant proc)
  (define h
    (if (list? variant)
        (apply make-immutable-hash variant)
        variant))

  ((for/fold ([wip proc])
             ([(s v) (in-hash h)])
     (λ ()
       (parameterize ([(setting-derived-parameter s) v])
         (wip))))))


(define (settings->flag-specs . settings)
  (map make-flag-spec settings))


(define-syntax (define-setting stx)
  (syntax-parse stx
    [(_ name:id cnt:expr short-flag:expr get-default:expr help-strs:expr)
     #'(define name (make-setting 'name get-default cnt short-flag help-strs))]))


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
    (define help-strings '("desc" "list"))
    (define-setting TAKES_LIST (list/c real? string?) "-L" '(1 "foo") help-strings)
    (check-eq? (setting-id TAKES_LIST) 'TAKES_LIST)
    (check-pred (setting-valid? TAKES_LIST) '(1 "foo"))
    (check-pred (negate (setting-valid? TAKES_LIST)) '("foo" 1))
    (check-pred parameter? (setting-parameter TAKES_LIST))
    (check-pred parameter? (setting-derived-parameter TAKES_LIST))
    (check-equal? (setting-short-flag TAKES_LIST) "-L")
    (check-equal? (setting-long-flag TAKES_LIST) "--TAKES_LIST")
    (check-equal? (setting-help-strings TAKES_LIST) help-strings)

    (test-exn "Reject invalid values"
              exn:fail:contract?
              (λ () (TAKES_LIST #f TAKES_LIST)))

    ; <!>
    (test-case "Parameterize new values"
      (check-equal? (TAKES_LIST '(2 "bar") TAKES_LIST) '(2 "bar"))
      (test-equal? "Old values are restored" (TAKES_LIST) '(1 "foo")))


    (test-case "Make flag specification for setting"
      (define spec (make-flag-spec TAKES_LIST))
      (match-define (list flags handler help) spec)
      (check-eq? (procedure-arity handler) 2)
      (check-equal? (handler "-L" "(2 \"bar\")") (cons TAKES_LIST '(2 "bar")))
      (check-equal? flags (list "-L" "--TAKES_LIST"))
      (check-equal? help help-strings)))

  (test-equal? "#f for a short flag means no short flag is defined"
    (car (make-flag-spec (setting 'FOO void void void #f "--FOO" '("desc" "val"))))
    '("--FOO"))

  (test-case "Define fallback values"
    (test-case "Compute fallback values using procedures"
      (define-setting NUM real? "-N" (const 1) '("number!" "<num>"))
      ; (void) represents an unset setting
      (check-equal? (NUM (void) NUM) 1))
    (test-case "Use exact fallback values"
      (define-setting NUM real? "-N" 1 '("number!" "<num>"))
      (check-equal? (NUM (void) NUM) 1))

    (test-exn "Validate computed fallback values"
              exn:fail:contract?
              (λ ()
                (define-setting NUM real? "-N" (const "not a number") '("number!" "<num>"))
                (NUM (void) NUM)))

    (test-exn "Validate exact fallback values"
              exn:fail:contract?
              (λ ()
                (define-setting NUM real? "-N" "not a number" '("number!" "<num>"))
                (NUM (void) NUM))))

  (define-setting GROUP_A boolean? "-a" #f '("Fuh" "#t-or-#f"))
  (define-setting GROUP_B boolean? "-b" #f '("Buh" "#t-or-#f"))
  (define-setting-group GROUP [GROUP_A GROUP_B])

  (test-true "Define settings in groups"
             (and (hash? GROUP)
                  (immutable? GROUP)
                  (eq? (hash-ref GROUP 'GROUP_A) GROUP_A)
                  (eq? (hash-ref GROUP 'GROUP_B) GROUP_B)))

  (test-case "Accumulate setting values using racket/cmdline"
    (define-setting FOO symbol? "-f" 'default '("A symbol" "symbol"))
    (define-setting BAR symbol? "-b" 'default '("A symbol" "symbol"))
    (define-setting ZAP real?   "-z"        0 '("A number" "number"))
    (define-setting DEF symbol? "-d" 'default '("A symbol" "symbol"))

    ; Notice DEF is added as a flag, but is not set in the command line.
    (parse-command-line "whatever"
                        #("--BAR" "y" "-f" "x" "-z" "1" "arg1" "arg2")
                        `((once-each . ,(settings->flag-specs FOO BAR ZAP DEF)))
                        (λ (flags 1st 2nd)
                          (check-equal? 1st "arg1")
                          (check-equal? 2nd "arg2")
                          (check-equal? flags
                                        (list (cons BAR 'y)
                                              (cons FOO 'x)
                                              (cons ZAP '1)))

                          (call-with-applied-settings flags
                           (λ ()
                             (check-eq? (BAR) 'y)
                             (check-eq? (FOO) 'x)
                             (check-eq? (ZAP) '1)
                             (check-eq? (DEF) 'default)))

                          ; Back to normal
                          (check-eq? (BAR) 'default)
                          (check-eq? (FOO) 'default)
                          (check-eq? (ZAP) 0)
                          (check-eq? (DEF) 'default))
                        '("arg1" "arg2"))))
