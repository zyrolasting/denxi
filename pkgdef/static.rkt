#lang racket/base

; Operate on package definitions as a syntax objects or lists.

(require (for-syntax racket/base
                     racket/match)
         racket/match
         "../codec.rkt"
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
          [make-input-expression
           (->* ((or/c path-string? input-port?)
                 (-> bytes? path-string? (non-empty-listof any/c))
                 string?
                 path-string?)
                (#:local-name string?
                 #:md-algorithm md-algorithm/c
                 #:byte-encoding (or/c #f xiden-encoding/c)
                 (or/c #f path-string?))
                list?)]
          [bare-pkgdef? flat-contract?]
          [get-static-abbreviated-query
           (-> bare-pkgdef? package-query?)]
          [get-static-inputs
           (-> bare-pkgdef? list?)]
          [get-static-simple-value
           (-> bare-pkgdef? symbol? any/c any/c)]
          [get-static-simple-string
           (-> bare-pkgdef? symbol? any/c)]
          [override-inputs
           (-> bare-pkgdef?
               list?
               bare-pkgdef?)]
          [autocomplete-inputs
           (->* (bare-racket-module?
                 #:public-key-source (or/c #f non-empty-string?)
                 #:private-key-path (or/c #f path-string?)
                 #:find-data procedure?)
                (#:default-name non-empty-string?
                 #:byte-encoding (or/c #f xiden-encoding/c)
                 #:default-md-algorithm md-algorithm/c
                 #:override-sources procedure?
                 #:private-key-password-path (or/c #f path-string?))
                bare-racket-module?)]
          [autocomplete-input-expression
           (->* (any/c
                 #:public-key-source (or/c #f non-empty-string?)
                 #:private-key-path (or/c #f path-string?)
                 #:find-data procedure?)
                (#:default-name non-empty-string?
                 #:byte-encoding (or/c #f xiden-encoding/c)
                 #:default-md-algorithm md-algorithm/c
                 #:override-sources procedure?
                 #:private-key-password-path (or/c #f path-string?))
                any/c)]
          [analyze-input-expression
           (-> any/c
               (-> (or/c #f non-empty-string?)
                   (or/c #f (listof non-empty-string?))
                   (or/c #f md-algorithm/c)
                   (or/c #f (or/c bytes?
                                  (list/c (or/c 'hex 'base32 'base64)
                                          (or/c bytes? string?))))
                   (or/c #f string?)
                   (or/c #f (or/c bytes?
                                  (list/c (or/c 'hex 'base32 'base64)
                                          (or/c bytes? string?))))
                   any)
               any)]))

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
  (get-static-simple-value stripped id "default"))

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

(define (make-input-expression
         path-or-port
         #:local-name [local-name (infer-local-name path-or-port)]
         #:byte-encoding [byte-encoding 'base64]
         #:md-algorithm [message-digest-algorithm 'sha384]
         make-sources
         public-key-source
         private-key-path
         [private-key-password-path #f])
  (let* ([digest (make-digest path-or-port message-digest-algorithm)]
         [make-byte-expression
          (if byte-encoding
              (λ (bstr) `(,byte-encoding ,(coerce-string (encode byte-encoding bstr))))
              values)]
         [generated-sources (make-sources digest path-or-port)])
    (if (and public-key-source private-key-path)
        `(input ,local-name
                (sources . ,generated-sources)
                (integrity ',message-digest-algorithm ,(make-byte-expression digest))
                (signature ,public-key-source
                           ,(make-byte-expression
                             (make-signature-bytes
                              digest
                              (expand-user-path private-key-path)
                              (and private-key-password-path
                                   (expand-user-path private-key-password-path))))))
        `(input ,local-name
                (sources . ,generated-sources)
                (integrity ',message-digest-algorithm ,(make-byte-expression digest))))))



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


;------------------------------------------------------------------------
; Input autocompletion

(define (autocomplete-inputs stripped
                             #:byte-encoding [byte-encoding 'base64]
                             #:default-md-algorithm [md-algorithm 'sha384]
                             #:override-sources [override-sources (λ (d p s) s)]
                             #:private-key-password-path [private-key-password-path #f]
                             #:default-name [default-name DEFAULT_STRING]
                             #:public-key-source public-key-source
                             #:find-data find-data
                             #:private-key-path private-key-path)
  (map (λ (form)
         (if (eq? 'input (car form))
             (autocomplete-input-expression form
                                            #:byte-encoding byte-encoding
                                            #:default-md-algorithm md-algorithm
                                            #:default-name default-name
                                            #:find-file find-data
                                            #:override-sources override-sources
                                            #:public-key-source public-key-source
                                            #:private-key-path private-key-path
                                            #:private-key-password-path private-key-password-path)
             form))
       (bare-racket-module-code stripped)))


(define (autocomplete-input-expression #:byte-encoding [byte-encoding 'base64]
                                       #:default-md-algorithm [default-md-algorithm 'sha384]
                                       #:override-sources [override-sources (λ (d p s) s)]
                                       #:private-key-password-path [private-key-password-path #f]
                                       #:default-name [default-name DEFAULT_STRING]
                                       #:find-data find-data
                                       #:private-key-path private-key-path
                                       #:public-key-source public-key-source
                                       expr)
  (analyze-input-expression expr
                            (λ (n s md ib pk sb)
                              (make-input-expression
                               (find-data n s md ib pk sb)
                               #:local-name (or n default-name)
                               #:byte-encoding byte-encoding
                               #:md-algorithm (or md default-md-algorithm)
                               (λ (d p) (override-sources d p s))
                               public-key-source
                               private-key-path
                               private-key-password-path))))


(define (analyze-input-expression expr continue)
  ; The use of two character long pattern variables makes it easier
  ; to visually line up arguments to continue and see if something
  ; is missing.
  (match expr
    [`(input)
     (continue #f #f #f #f #f #f)]

    [`(input ,(nexpr nm))
     (continue nm #f #f #f #f #f)]

    [`(input ,(nexpr nm) ,(srcexpr sr))
     (continue nm sr #f #f #f #f)]

    [`(input ,(nexpr nm) ,(srcexpr sr) (integrity))
     (continue nm sr #f #f #f #f)]

    [`(input ,(nexpr nm) ,(srcexpr sr) (integrity ',(mdexpr md)))
     (continue nm sr `',md #f #f #f)]

    [`(input ,(nexpr nm) ,(srcexpr sr) (integrity ',(mdexpr md) ,(bexpr ib)))
     (continue nm sr `',md ib #f #f)]

    [`(input ,(nexpr nm) ,(srcexpr sr) (integrity ',(mdexpr md) ,(bexpr ib)) (signature))
     (continue nm sr `',md ib #f #f)]

    [`(input ,(nexpr nm) ,(srcexpr sr) (integrity ',(mdexpr md) ,(bexpr ib)) (signature ,(pkexpr pk)))
     (continue nm sr `',md ib pk #f)]

    [`(input ,(nexpr nm) ,(srcexpr sr) (integrity ',(mdexpr md) ,(bexpr ib)) (signature ,(pkexpr pk) ,(bexpr sb)))
     (continue nm sr `',md ib pk sb)]

    [_ expr]))


(define-match-expander mdexpr
  (λ (stx)
    (syntax-case stx ()
      [(_ id)
       #'(? md-algorithm/c id)])))

(define-match-expander pkexpr
  (λ (stx)
    (syntax-case stx ()
      [(_ id)
       #'(? string? id)])))

(define-match-expander bexpr
  (λ (stx)
    (syntax-case stx ()
      [(_ id)
       #'(or (? bytes? id)
             (? (λ (v)
                  (match v
                    [`(,(or 'hex 'base64 'base32)
                       ,(? (or/c bytes? string?) _))
                     #t]
                    [_ #f])) id))])))

(define-match-expander nexpr
  (λ (stx)
    (syntax-case stx ()
      [(_ id)
       #'(? non-empty-string? id)])))

(define-match-expander srcexpr
  (λ (stx)
    (syntax-case stx ()
      [(_ id)
       #'(or (? (listof non-empty-string?) id)
             `(sources ,(? non-empty-string? id) ___))])))

(module+ test
  (require rackunit)

  (test-case "Analyze input expressions"
    (define (check e v)
      (check-equal? (analyze-input-expression e list) v))
    (check '(input) '(#f #f #f #f #f #f))
    (check '(input "a") '("a" #f #f #f #f #f))
    (check '(input "a" (sources)) '("a" () #f #f #f #f))
    (check '(input "a" ()) '("a" () #f #f #f #f))
    (check '(input "a" (sources "foo")) '("a" ("foo") #f #f #f #f))
    (check '(input "a" ("foo")) '("a" ("foo") #f #f #f #f))
    (check '(input "a" ("b" "c") (integrity 'sha1)) '("a" ("b" "c") 'sha1 #f #f #f))
    (check '(input "a" ("b" "c") (integrity 'sha1 #"")) '("a" ("b" "c") 'sha1 #"" #f #f))
    (check '(input "a" ("b" "c") (integrity 'sha1 (hex ""))) '("a" ("b" "c") 'sha1 (hex "") #f #f))
    (check '(input "a" ("b" "c") (integrity 'sha1 #"") (signature "pk")) '("a" ("b" "c") 'sha1 #"" "pk" #f))
    (check '(input "a" ("b" "c") (integrity 'sha1 #"") (signature "pk" #"")) '("a" ("b" "c") 'sha1 #"" "pk" #""))
    (check '(input "a" ("b" "c") (integrity 'sha1 #"") (signature "pk" (base32 #""))) '("a" ("b" "c") 'sha1 #"" "pk" (base32 #""))))

  (test-case "Do not analyze input expressions when terms look incorrect"
    (define (check e)
      (check-eq? (analyze-input-expression e list) e))
    ; The number 1 is a dummy incorrect datum I just use because it's convenient.
    (check '(input 1))
    (check '(input ""))
    (check '(input "n" 1))
    (check '(input "n" () 1))
    (check '(input "n" () (integrity 1)))
    (check '(input "n" () (integrity sha1)))
    (check '(input "n" () (integrity ''sha1)))
    (check '(input "n" () (integrity 'sha1 1)))
    (check '(input "n" () (integrity 'sha1 #"" 1)))
    (check '(input "n" () (integrity 'sha1 #"") (signature 1)))
    (check '(input "n" () (integrity 'sha1 #"") (signature "pk" 1)))
    (check '(input "n" () (integrity 'sha1 #"") (signature "pk" #"" 1))))

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
       (input "b")
       (input "c")
       (input "c"))))

  (test-equal? "Extract simple string"
               (get-static-simple-string original 'package)
               "alpha")

  (test-equal? "Extract default string"
               (get-static-simple-string original 'provider)
               "default")

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
                     (input "b")
                     (input "c" (integrity 'sha1 "foo"))
                     (input "c" (integrity 'sha1 "foo")))))))
