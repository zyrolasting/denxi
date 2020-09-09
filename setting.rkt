#lang racket/base

; Define a setting as a single dynamically-bound Racket value derived
; from several configuration sources. Protect each setting with a
; contract.

(require "contract.rkt")
(provide (struct-out setting)
         define-setting
         define-setting-group
         (contract-out
          [setting-short-flag
           (-> setting? string?)]
          [setting-format-all-flags
           (-> setting? string?)]
          [call-with-applied-settings
           (-> (if/c hash?
                     (hash/c setting? any/c)
                     (listof (cons/c setting? any/c)))
               (-> any)
               any)]
          [settings->flag-specs
           (->* () #:rest (listof setting?) list?)]))


(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         "string.rkt"
         "url.rkt")


(struct setting (id valid? parameter derived-parameter flag-kind flag-strings help-strings)
  #:property prop:procedure
  (case-lambda [(self) ((setting-derived-parameter self))]
               [(self v proc) (parameterize ([(setting-derived-parameter self) v]) (proc))]))


(define (setting-short-flag s)
  (for/fold ([shortest (car (setting-flag-strings s))])
            ([next (in-list (cdr (setting-flag-strings s)))])
    (if (< (string-length next) (string-length shortest))
        next
        shortest)))

(define (setting-format-all-flags s)
  (string-join
   (sort (setting-flag-strings s)
         < #:key string-length)
   "/"))


(define (make-setting id-sym get-default valid? flag-kind flag-strings help-strings)
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
           flag-kind
           (cons (format "--~a" id-sym) flag-strings)
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
  (list (setting-flag-strings s)
        (λ (flag user-asserted-value)
          (define v (read (open-input-string user-asserted-value)))
          (cons s (if (symbol? v) user-asserted-value v)))
        (setting-help-strings s)))


; When using a setting as a (multi) flag in parse-command-line,
; this will accumulate repeated values for a setting into lists.
(define (fold-setting-values may-contain-multis)
  (for/fold ([wip (hasheq)])
            ([pair (in-list may-contain-multis)])
    (define setting-inst (car pair))
    (define setting-val  (cdr pair))
    (hash-set wip setting-inst
              (if (eq? 'multi (setting-flag-kind setting-inst))
                  (cons setting-val
                        (hash-ref wip setting-inst null))
                  setting-val))))

; Calls proc in a parameterization where an alist or hash
; determines a new value for each setting.
(define (call-with-applied-settings variant proc)
  (define h
    (fold-setting-values
     (if (list? variant)
         variant
         (hash->list variant))))

  ((for/fold ([wip proc])
             ([(s v) (in-hash h)])
     (λ ()
       (parameterize ([(setting-derived-parameter s) v])
         (wip))))))

(define (fold-flag-kinds h settings)
  (if (null? settings)
      h
      (let ([s (car settings)])
        (fold-flag-kinds
         (hash-set h
                   (setting-flag-kind s)
                   (cons (make-flag-spec s)
                         (hash-ref h (setting-flag-kind s) null)))
         (cdr settings)))))


(define (settings->flag-specs . settings)
  (for/list ([(kind grouped) (in-hash (fold-flag-kinds (hasheq) settings))])
    (cons kind (reverse grouped))))


(define-syntax (define-setting stx)
  (syntax-parse stx
    [(_ name:id cnt:expr kind:expr flag-strings:expr get-default:expr help-strs:expr)
     #'(define name (make-setting 'name get-default cnt kind flag-strings help-strs))]))


(define-syntax-rule (define-setting-group id (s ...))
  (define id (make-immutable-hash (list (cons (setting-id s) s) ...))))


(module+ test
  (require racket/function
           racket/cmdline
           racket/match
           rackunit)

  (test-case "Fold setting values"
    (define-setting A real? 'multi '("-A") #f '("blah" "blah"))
    (define-setting B real? 'once-each '("-B") #f '("blah" "blah"))

    (check-equal? (fold-setting-values (list (cons A 1) (cons A 5) (cons B 10) (cons B 12)))
                  (hasheq A '(5 1)
                          B 12)))

  ; Below you'll see a pattern of the form (SETTING <value> SETTING).
  ; (SETTING <value> <proc>) means "Call <proc> with SETTING bound to
  ; <value>". Since (SETTING) returns the current value of the setting
  ; for the current parameterization, (SETTING <value> SETTING) is a
  ; way to rebind SETTING, return the set value, then restore the
  ; original value in a single expression. See the test marked with
  ; <!> for an example of this.

  (test-case "Define a setting"
    (define help-strings '("desc" "list"))
    (define-setting TAKES_LIST (list/c real? string?) 'once-each '("-L") '(1 "foo") help-strings)
    (check-eq? (setting-id TAKES_LIST) 'TAKES_LIST)
    (check-pred (setting-valid? TAKES_LIST) '(1 "foo"))
    (check-pred (negate (setting-valid? TAKES_LIST)) '("foo" 1))
    (check-pred parameter? (setting-parameter TAKES_LIST))
    (check-pred parameter? (setting-derived-parameter TAKES_LIST))
    (check-equal? (setting-flag-strings TAKES_LIST) '("--TAKES_LIST" "-L"))
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
      (check-equal? flags (list  "--TAKES_LIST" "-L"))
      (check-equal? help help-strings)))

  (test-equal? "Always include longform flag"
    (car (make-flag-spec (setting 'FOO void void void 'once-each '("--FOO") '("desc" "val"))))
    '("--FOO"))

  (test-case "Define fallback values"
    (test-case "Compute fallback values using procedures"
      (define-setting NUM real? 'once-each '("-N") (const 1) '("number!" "<num>"))
      ; (void) represents an unset setting
      (check-equal? (NUM (void) NUM) 1))
    (test-case "Use exact fallback values"
      (define-setting NUM real? 'once-each '("-N") 1 '("number!" "<num>"))
      (check-equal? (NUM (void) NUM) 1))

    (test-exn "Validate computed fallback values"
              exn:fail:contract?
              (λ ()
                (define-setting NUM real? 'once-each '("-N") (const "not a number") '("number!" "<num>"))
                (NUM (void) NUM)))

    (test-exn "Validate exact fallback values"
              exn:fail:contract?
              (λ ()
                (define-setting NUM real? 'once-each '("-N") "not a number" '("number!" "<num>"))
                (NUM (void) NUM))))

  (define-setting GROUP_A boolean? 'once-each '("-a") #f '("Fuh" "#t-or-#f"))
  (define-setting GROUP_B boolean? 'once-each '("-b") #f '("Buh" "#t-or-#f"))
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
               1)


  (test-case "Accumulate setting values using racket/cmdline"
    (define-setting FOO symbol? 'once-each '("-f") 'default '("A symbol" "symbol"))
    (define-setting BAR symbol? 'once-each '("-b") 'default '("A symbol" "symbol"))
    (define-setting ZAP real?   'once-each '("-z")        0 '("A number" "number"))
    (define-setting DEF symbol? 'once-each '("-d") 'default '("A symbol" "symbol"))

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
