#lang racket/base

; Define a setting as a singular Racket value derived from the
; following configuration sources, where each item overrides the one
; before.
;
; 1. A hard-coded default
; 2. A runtime configuration file
; 3. An environment variable
; 4. A command line argument
;
; Protect each setting with a contract.
;
; Assuming the name of a setting is "Use Widgets", the following
; must hold:
;
; - The envvar's name is `ZCPKG_USE_WIDGETS`.
;
; - (closure 'ZCPKG_USE_WIDGETS) produces a value.
;
; - The command line flag is --use-widgets "...", where "..." is a
;   (read)able value. If the setting is a boolean, omit the "...".

(provide (struct-out setting)
         define-setting
         define-setting-group
         current-setting-value-lookup
         settings->flag-specs)

(require "contract.rkt"
         "string.rkt"
         "url.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx))

(define current-setting-value-lookup (make-parameter void))

(struct setting
  (id
   valid?
   make-flag-spec
   default-value
   [override-value #:mutable])
  #:property
  prop:procedure
  (case-lambda
    [(self)
     (define id (setting-id self))
     (with-handlers ([exn:fail:contract?
                      (λ (e) (raise (rewrite-contract-error-message e id)))])
       (invariant-assertion
        (setting-valid? self)
        (or (for/or ([get (in-list
                           (list (λ () (setting-override-value self))
                                 (λ () ((current-setting-value-lookup) id))
                                 (λ ()
                                   (define env (getenv (symbol->string id)))
                                   (cond [(not env) (void)]
                                         [(string=? env "") (void)]
                                         [else (read (open-input-string env))]))))])
              (define v (get))
              (and (not (void? v)) v))
            (setting-default-value self))))]
    [(self v)
     (set-setting-override-value!
      self
      (invariant-assertion (setting-valid? self) v))
     self]))

(define (flag-names short id)
  (let ([long (setting-id->cli-flag-string id)])
    (if short
        (list short long)
        (list long))))


(define (setting-id->cli-flag-string id)
  (string-append "--"
                 (string-downcase
                  (string-join (cdr (string-split (symbol->string id) "_"))
                               "-"))))


(define (make-flag-spec-closure cnt help-strs flags)
  (λ (s)
    (list flags
          (if (eq? cnt boolean?)
              (λ (flag) (s #t))
              (λ (flag v) (s (read (open-input-string v)))))
          help-strs)))


(define (setting->short-flag s)
 (caar (setting->flag-spec s)))


(define (setting->long-flag s)
 (cadar (setting->flag-spec s)))


(define (setting->flag-spec s)
  ((setting-make-flag-spec s) s))


(define (settings->flag-specs . settings)
  (map setting->flag-spec settings))


(define-syntax (define-setting stx)
  (syntax-parse stx
    [(_ name:id short-flag:expr (str:string ...) cnt:expr default:expr)
     #'(define name
         (setting 'name
                  cnt
                  (make-flag-spec-closure cnt
                                          (list str ...)
                                          (flag-names short-flag 'name))
                  default
                  (void)))]))

(define-syntax (define-setting-group stx)
  (syntax-parse stx
    [(_ id [name:id short-flag:expr (str:string ...) cnt:expr default:expr] ...)
     #'(begin (begin (define-setting name short-flag (str ...) cnt default) ...)
              (define id (make-immutable-hash (list (cons 'name name) ...))))]))


(module+ test
  (require rackunit
           racket/cmdline)

  (define-setting ZCPKG_VERBOSE "-v" ("Show debug logs") boolean? #f)

  (void (putenv "PUMP_LEVEL" ""))

  (define-setting PUMP_LEVEL "-P"
    ("Designate how pumped you are in standard urrrg units"
      "pump-level")
    real?
    100)

  (test-true "Given an identifier, the instance bound to it has a symbol of that identifier"
             (and (eq? (setting-id ZCPKG_VERBOSE) 'ZCPKG_VERBOSE)
                  (eq? (setting-id PUMP_LEVEL) 'PUMP_LEVEL)))

  (test-equal? "Allow user to define short flags"
               (setting->short-flag ZCPKG_VERBOSE)
               "-v")

  (test-equal? "Derive long flags from macro"
               (setting->long-flag ZCPKG_VERBOSE)
               "--verbose")

  (test-equal? "PUMP_LEVEL CLI flags are derived from macro"
               (car (setting->flag-spec PUMP_LEVEL))
               '("-P" "--level"))

  (test-eq? "The default value comes before all other sources"
            (PUMP_LEVEL)
            100)

  (test-eq? "Define the ZCPKG_VERBOSE flag handler as arity 1, for handling a boolean flag"
            (procedure-arity (cadr (setting->flag-spec ZCPKG_VERBOSE)))
            1)

  (test-eq? "Define the PUMP_LEVEL flag handler as arity 2, for handling a flag bound to an option"
            (procedure-arity (cadr (setting->flag-spec PUMP_LEVEL)))
            2)

  (void (putenv "PUMP_LEVEL" "20"))
  (test-eq? "A readable envvar value overrides the default value." (PUMP_LEVEL) 20)

  (parameterize ([current-setting-value-lookup
                  (λ (k) (if (eq? k 'PUMP_LEVEL) 99 (void)))])
    (test-eq? "A parameter overrides an envar." (PUMP_LEVEL) 99)
    (test-eq? "A per-setting override has highest authority." ((PUMP_LEVEL 0)) 0))

  (test-exn "Guard against setting invalid override values"
            exn:fail:contract?
            (λ () (ZCPKG_VERBOSE 'blah)))

  (void (putenv "PUMP_LEVEL" "wooooo"))
  (test-exn "Guard against reading invalid values from elsewhere"
            exn:fail:contract?
            (λ () (PUMP_LEVEL)))

  (define-setting-group GROUP
    [GROUP_A "-a" ("Fuh") boolean? #f]
    [GROUP_B "-b" ("Buh") boolean? #f])

  (test-true "Define settings in groups"
             (and (hash? GROUP)
                  (immutable? GROUP)
                  (eq? (hash-ref GROUP 'GROUP_A) GROUP_A)
                  (eq? (hash-ref GROUP 'GROUP_B) GROUP_B))))
