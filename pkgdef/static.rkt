#lang racket/base

; Operate on package definitions as syntax objects or lists.

(require "../codec.rkt"
         "../contract.rkt"
         "../logged.rkt"
         "../openssl.rkt"
         "../path.rkt"
         "../query.rkt"
         "../racket-module.rkt"
         "../signature.rkt"
         "../string.rkt")

(provide (contract-out
          [PACKAGE_DEFINITION_MODULE_LANG symbol?]
          [PACKAGE_DEFINITION_READER_LANG symbol?]
          [package-definition-datum? predicate/c]
          [get-package-definition-body
           (-> package-definition-datum? list?)]
          [make-package-definition-datum
           (->* (list?) (#:id symbol?) package-definition-datum?)]
          [read-package-definition
           (->* (racket-module-input-variant/c)
                (logged/c syntax?))]
          [bare-pkgdef? flat-contract?]
          [get-static-abbreviated-query
           (-> bare-pkgdef? package-query?)]
          [get-static-inputs
           (-> bare-pkgdef? list?)]
          [get-static-simple-value
           (-> bare-pkgdef? symbol? any/c any/c)]
          [get-static-list-value
           (-> bare-pkgdef? symbol? any/c any/c)]
          [get-static-simple-string
           (-> bare-pkgdef? symbol? any/c)]
          [override-inputs
           (-> bare-pkgdef?
               list?
               bare-pkgdef?)]))

(define PACKAGE_DEFINITION_MODULE_LANG 'xiden/pkgdef)
(define PACKAGE_DEFINITION_READER_LANG 'xiden)

(define (package-definition-datum? v)
  (racket-module-code? PACKAGE_DEFINITION_MODULE_LANG v))

(define bare-pkgdef?
  (struct/c bare-racket-module
            symbol?
            PACKAGE_DEFINITION_MODULE_LANG
            list?))

(define (read-package-definition variant)
  (read-racket-module PACKAGE_DEFINITION_READER_LANG PACKAGE_DEFINITION_MODULE_LANG variant))

(define (make-package-definition-datum #:id [id 'pkgdef] body)
  (make-racket-module-datum #:id id PACKAGE_DEFINITION_MODULE_LANG body))

(define (get-package-definition-body datum)
  (get-racket-module-body PACKAGE_DEFINITION_MODULE_LANG datum))


;-------------------------------------------------------------------------------
; Static analysis

(define (get-static-input-name stx)
  (syntax-case stx (input) [(input name . xs) (syntax-e #'name)] [_ #f]))

(define (get-static-output-name stx)
  (syntax-case stx (output) [(output name . xs) (syntax-e #'name)] [_ #f]))

(define (get-static-simple-value stripped id default)
  (let loop ([next (bare-racket-module-code stripped)])
    (syntax-case next ()
      [((actual-id val) . xs)
       (equal? (syntax-e #'actual-id) id)
       (syntax-e #'val)]
      [(x . xs)
       (loop #'xs)]
      [_ default])))

(define (get-static-list-value stripped id default)
  (let loop ([next (bare-racket-module-code stripped)])
    (syntax-case next ()
      [((actual-id . vs) . xs)
       (equal? (syntax-e #'actual-id) id)
       (syntax->datum #'vs)]
      [(x . xs)
       (loop #'xs)]
      [_ default])))

(define (syntax-filter-map f code [found null])
  (syntax-case code ()
    [() (reverse found)]
    [(x . xs)
     (let ([v (f #'x)])
       (if v
           (syntax-filter-map f #'xs (cons v found))
           (syntax-filter-map f #'xs found)))]))

(define (bind-filter-map-proc/keep-datum f)
  (λ (v) (and (f v)
              (syntax->datum v))))

(define (get-static-inputs stripped)
  (syntax-filter-map (bind-filter-map-proc/keep-datum get-static-input-name)
                     (bare-racket-module-code stripped)))

(define (get-static-outputs stripped)
  (syntax-filter-map (bind-filter-map-proc/keep-datum get-static-output-name)
                     (bare-racket-module-code stripped)))

(define (get-static-simple-string stripped id)
  (get-static-simple-value stripped id DEFAULT_STRING))

(define (get-static-abbreviated-query stripped)
  (format "~a:~a:~a:~a"
          (get-static-simple-string stripped 'provider)
          (get-static-simple-string stripped 'package)
          (get-static-simple-string stripped 'edition)
          (get-static-simple-value  stripped 'revision-number 0)))


;-------------------------------------------------------------------------------
; Authoring aids

(define (infer-local-name path-or-port)
  (if (path-string? path-or-port)
      (path->string (file-name-from-path path-or-port))
      (or (object-name path-or-port)
          "")))

;------------------------------------------------------------------------
; Input overriding

(define (override-inputs stripped input-exprs)
  (if (null? input-exprs)
      stripped
      (let* ([input-expr (car input-exprs)]
             [expected-name (get-static-input-name input-expr)])
        (override-inputs
         (struct-copy bare-racket-module stripped
                      [code
                       (for/list ([form (in-list (bare-racket-module-code stripped))])
                         (if (equal? (get-static-input-name form) expected-name)
                             input-expr
                             form))])
         (cdr input-exprs)))))


(define-logged (read-package-query stripped defaults)
  (define (parse v) (get-static-simple-value stripped v))
  (define provider
    (parse 'provider (get-default-provider defaults)))
  (define package
    (parse 'package (get-default-package defaults provider)))
  (define edition
    (parse 'edition (get-default-edition defaults provider package)))
  (define revision-min
    (number->string
     (parse 'revision-number
            (get-default-min-revision
             defaults
             provider
             package
             edition))))
  (define revision-max
    (number->string
     (parse 'revision-number
            (get-default-max-revision
             defaults
             provider
             package
             edition
             revision-min))))
  ($use (parsed-package-query provider
                              package
                              edition
                              revision-min
                              revision-max
                              "ii")))


(define (get-package-definition-revision-names stx)
  (syntax-case stx (revision-names)
    [((revision-names . names) . xs)
     (syntax->datum #'names)]
    [(x . xs)
     (get-package-definition-revision-names #'xs)]
    [_
     null]))





(module+ test
  (require rackunit)

  (test-case "Extract input name"
    (define (check . forms)
      (for ([form (in-list forms)])
        (test-case (format "Extract input name from ~s" form)
          (check-equal? (get-static-input-name form) "a")
          (check-equal? (get-static-input-name (syntax->datum form)) "a"))))

    (check #'(input "a")
           #'(input "a"
                    (integrity 'sha384 (hex "...")))
           #'(input "a"
                    (integrity 'sha384 (hex "..."))
                    (signature "" ""))))

  (test-false "Only extract input names from input forms"
              (get-static-input-name #'(output "x")))

  (test-case "Filter elements from syntax list or normal list"
    (check-pred null? (syntax-filter-map (λ _ #f) null))
    (check-pred null? (syntax-filter-map (λ _ #f) '(1 2 3)))

    (let ([lst #'(1 2 3)])
      (check-equal?
       (syntax-filter-map (λ (v) v) lst)
       (syntax-e lst)))

    (check-equal?
     (syntax-filter-map (λ (v) (format "~a" (syntax-e v))) #'(a b c))
     '("a" "b" "c")))

  (define original
    (bare-racket-module
     'anon
     PACKAGE_DEFINITION_MODULE_LANG
     '((package "alpha")
       (input "a")
       (edition "cool")
       (revision-names "x" "y" "z")
       (input "b")
       (input "c")
       (input "c"))))

  (test-equal? "Extract simple string"
               (get-static-simple-string original 'package)
               "alpha")

  (test-equal? "Extract default string"
               (get-static-simple-string original 'provider)
               DEFAULT_STRING)

  (test-equal? "Extract listed terms"
               (get-static-list-value original 'revision-names null)
               '("x" "y" "z"))

  (test-equal? "Find input expressions"
               (get-static-inputs original)
               '((input "a")
                 (input "b")
                 (input "c")
                 (input "c")))

  (test-case "Override inputs"
    (check-eq? (override-inputs original null)
               original)

    (check-equal? (override-inputs
                   original
                   '((input "a" (integrity 'sha384 "blah"))
                     (input "c" (integrity 'sha1 "foo"))))
                  (bare-racket-module
                   'anon
                   PACKAGE_DEFINITION_MODULE_LANG
                   '((package "alpha")
                     (input "a" (integrity 'sha384 "blah"))
                     (edition "cool")
                     (revision-names "x" "y" "z")
                     (input "b")
                     (input "c" (integrity 'sha1 "foo"))
                     (input "c" (integrity 'sha1 "foo")))))))
