#lang racket/base

; Logged procedures for static operations on Racket modules

(require (only-in racket/exn
                  exn->string)
         (only-in racket/format
                  ~s)
         (only-in racket/function
                  curry)
         (only-in syntax/modread
                  check-module-form
                  with-module-reading-parameterization)
         syntax/parse
         "contract.rkt"
         "input.rkt"
         "logged.rkt"
         "message.rkt"
         "monad.rkt"
         "system.rkt")

(provide
 (struct-out bare-racket-module)
 (contract-out [racket-module-input-variant/c flat-contract?]
               [racket-module-variant/c flat-contract?]
               [code/c flat-contract?]
               [coerce-datum (-> code/c list?)]
               [list/syntax? predicate/c]
               [any-racket-module-code? predicate/c]
               [racket-module-code? (-> symbol? any/c boolean?)]
               [strip (-> racket-module-variant/c bare-racket-module?)]
               [dress (-> bare-racket-module? list?)]
               [get-racket-module-body
                (-> (or/c #f symbol?) racket-module-variant/c (or/c #f code/c))]
               [make-racket-module-datum
                (->* (symbol? list?)
                     (#:id symbol?)
                     list?)]
               [read-racket-module
                (->* (symbol? symbol? racket-module-input-variant/c)
                     (logged/c syntax?))]
               [keep-standalone-racket-module
                (->* (string?) (#:compile-with (or/c path-string? #f))
                     logged?)]))

(define+provide-message $racket-module-read-error $message (variant reason context))

;------------------------------------------------------------------------
; Definitions

(define (racket-module-code? expected-lang v)
  (and (code/c v)
       (syntax-case v (module)
         [(module name lang . body)
          (or (not expected-lang)
              (equal? (syntax-e #'lang) expected-lang))
          #t]
         [_ #f])))


(define (list/syntax? stx)
  (and (syntax? stx)
       (list? (syntax-e stx))))

(define (any-racket-module-code? v)
  (racket-module-code? #f v))

(define racket-module-input-variant/c
  (or/c path? list? string? bytes? input-port?))

(define code/c
  (or/c list/syntax? list?))

(define racket-module-variant/c
  (and/c code/c any-racket-module-code?))

(define (coerce-datum code)
  (if (syntax? code)
      (syntax->datum code)
      code))

;------------------------------------------------------------------------
; Module I/O

(define (read-racket-module expected-reader-lang expected-module-lang variant)
  (cond [(input-port? variant)
         (read-racket-module/port expected-reader-lang expected-module-lang variant)]
        [(path? variant)
         (read-racket-module/path expected-reader-lang expected-module-lang variant)]
        [(list? variant)
         (read-racket-module expected-reader-lang expected-module-lang (~s variant))]
        [(string? variant)
         (read-racket-module expected-reader-lang expected-module-lang (open-input-string variant))]
        [(bytes? variant)
         (read-racket-module expected-reader-lang expected-module-lang (open-input-bytes variant))]))


(define-logged (read-racket-module/path expected-reader-lang expected-module-lang variant)
  (call-with-input-file variant
    (λ (i)
      ($run! (read-racket-module/port expected-reader-lang expected-module-lang i)))))


(define-logged (read-racket-module/port expected-reader-lang expected-module-lang in)
  (define source-v (get-source-v in))
  (with-handlers ([procedure? (λ (p) ($fail (p source-v)))]
                  [exn? (λ (e) ($fail ($racket-module-read-error source-v 'exception (exn->string e))))])
    (port-count-lines! in)

    (define checked
      (check-module-form (read-module expected-reader-lang in)
                         'this-symbol-is-ignored
                         source-v))

    (syntax-case checked ()
      [(module id ml xs ...)
       (let ([module-lang (syntax-e #'ml)])
         (if (equal? module-lang expected-module-lang)
             ($use checked)
             ($fail ($racket-module-read-error source-v 'unexpected-module-lang module-lang))))]
      [_ ($fail ($racket-module-read-error source-v 'bad-module-form #f))])))


; Returns a value suitable for use as a `src' argument in
; `read-syntax', or a `source-v' argument in `check-module-form'.
(define (get-source-v in)
  (define objname (object-name in))
  (if (and (path-string? objname)
           (file-exists? objname))
      objname
      #f))


(define (read-module expected-reader-lang in)
  (with-module-reading-parameterization
    (λ ()
      (parameterize ([current-reader-guard (curry reader-guard expected-reader-lang)])
        (read-syntax (get-source-v in)
                     in)))))


(define (reader-guard lang reader)
  (if (equal? reader `(submod ,lang reader))
      reader
      (raise (λ (source-v) ($racket-module-read-error source-v 'blocked-reader reader)))))


;------------------------------------------------------------------------
; Construction/destructuring

(define (make-racket-module-datum #:id [id 'anon] lang body)
  `(module ,id ,lang . ,body))


(define (get-racket-module-body expected-lang variant)
  (define stx
    (syntax-case variant (module)
      [(module _ lang (_ . body))
       (or (not expected-lang)
           (equal? (syntax-e #'lang) expected-lang))
       #'body]
      [(module _ lang . body)
       (or (not expected-lang)
           (equal? (syntax-e #'lang) expected-lang))
       #'body]
      [_ #f]))
  (and stx
       (if (syntax? variant)
           stx
           (syntax->datum stx))))


;------------------------------------------------------------------------
; Strip/Dress

(struct bare-racket-module (name lang code) #:transparent)

(define (dress stripped)
  (make-racket-module-datum #:id
                            (bare-racket-module-name stripped)
                            (bare-racket-module-lang stripped)
                            (bare-racket-module-code stripped)))

(define (strip module-datum)
  (define (construct name lang body)
    (bare-racket-module (syntax-e name) (syntax-e lang) (syntax->datum body)))
  (syntax-parse module-datum
    #:datum-literals (module)
    [(module name:id lang:id (bodyform:id . body))
     #:when (member (syntax-e #'bodyform) '(#%module-begin #%plain-module-begin))
     (construct #'name #'lang #'body)]
    [(module name:id lang:id . body)
     (construct #'name #'lang #'body)]))


;------------------------------------------------------------------------
; For package definitions

(define (keep-standalone-racket-module #:compile-with [compile-with "raco"] name)
  (mdo input-path := (keep-input name)
       (if compile-with
           (run compile-with "make" input-path)
           (logged-unit input-path))))


(module+ test
  (require racket/file
           racket/function
           racket/port
           rackunit
           (submod "logged.rkt" test))

  (test-case "Detect code types"
    (check-true (code/c '()))
    (check-true (code/c #'()))
    (check-true (code/c '(1 2 3)))
    (check-true (code/c #'(1 2 3)))
    (check-false (code/c 1))
    (check-false (code/c #'"1")))

  (define (expect-bad-module-form val messages)
    (check-equal? val FAILURE)
    (check-match (car messages)
                 ($racket-module-read-error _ 'bad-module-form _)))

  (test-case "Detect Racket module data"
    (check-true (any-racket-module-code? (make-racket-module-datum 'something '(a b c))))
    (check-true (racket-module-code? 'something (make-racket-module-datum 'something null)))
    (check-false (racket-module-code? 'something '(module anon other)))
    (check-false (racket-module-code? 'something `(module something)))
    (check-false (racket-module-code? 'something '(module anon other 1 2 3))))

  (test-case "Extract body from module forms"
    (check-equal?  (get-racket-module-body  'something
                   `(module anon something a b c))
                  '(a b c))
    (check-equal? (get-racket-module-body 'something
                   `(module anon something (#%module-begin a b c)))
                  '(a b c))
    (check-equal? (get-racket-module-body 'something
                   (make-racket-module-datum 'something '(a b c)))
                  '(a b c)))

  (test-logged-procedure "Detect error when reading improper module form"
                         (read-racket-module '_ '_ "(module)")
                         expect-bad-module-form)

  (test-logged-procedure "Detect if EOF came too soon"
                         (read-racket-module '_ '_ "")
                         expect-bad-module-form)

  (test-logged-procedure "Read with reader extension"
                         (read-racket-module 'racket/base
                                             'racket/base
                                             "#lang racket/base (define val 1)")
                         (λ (v msg)
                           (check-pred syntax? v)
                           (check-match (syntax->datum v)
                                        `(module ,_ racket/base (,_ (define val 1))))
                           (check-pred null? msg)))

  (test-logged-procedure "Accept only prescribed reader extensions"
                         (read-racket-module 'other 'racket/base "#lang racket/base (define val 1)")
                         (λ (v msg)
                           (check-eq? v FAILURE)
                           (check-match (car msg)
                                        ($racket-module-read-error #f
                                                                   'blocked-reader
                                                                   '(submod racket/base reader)))))

  (test-logged-procedure "Accept only prescribed expander language"
                         (read-racket-module 'racket/base
                                             'other
                                             "#lang racket/base (define val 1)")
                         (λ (v msg)
                           (check-eq? v FAILURE)
                           (check-match (car msg)
                                        ($racket-module-read-error _ 'unexpected-module-lang _))))

  (test-case "Strip and dress modules"
    (define body '((define a 1) (provide a)))
    (check-equal? (strip #`(module anon racket/base (#%module-begin . #,body)))
                  (bare-racket-module 'anon 'racket/base body))
    (check-equal? (strip #`(module anon racket/base (#%plain-module-begin . #,body)))
                  (bare-racket-module 'anon 'racket/base body))
    (check-equal? (strip #`(module anon racket/base . #,body))
                  (bare-racket-module 'anon 'racket/base body))))
