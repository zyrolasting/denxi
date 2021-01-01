#lang racket/base

; Module language for package definitions

(provide #%app
         #%datum
         #%top-interaction
         :=
         all-defined-out
         base32
         base64
         current-directory
         define
         define-values
         description
         edition
         except-out
         extract
         find-input
         from-catalogs
         from-file
         hex
         in-paths
         input
         input-ref
         integrity
         list
         logged-unit
         mdo
         metadatum
         os-support
         output
         package
         provide
         provider
         quote
         racket-versions
         release-input
         resolve-input
         revision-names
         revision-number
         run
         signature
         sources
         tags
         url
         (rename-out [#%module-begin* #%module-begin]))

(require (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context
                     syntax/parse/lib/function-header
                     racket/syntax
                     (only-in version/utils
                              valid-version?
                              version<=?)
                     (only-in "racket-version.rkt"
                              racket-version-selection)
                     (only-in "string.rkt"
                              non-empty-string
                              non-empty-string?
                              string-join)
                     (only-in "url.rkt"
                              url-string))
         syntax/parse/define
         "archive.rkt"
         "codec.rkt"
         "file.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "logged.rkt"
         "monad.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "source.rkt"
         "system.rkt")



;-------------------------------------------------------------------------------
; Collects information from the module

(struct pkgdef-state
  (provider
   package
   edition
   revision-number
   revision-names
   racket-versions
   metadata
   inputs
   build
   output-names
   tags
   description
   url
   os-support)
  #:transparent)

(define ALL_OS_SYMS '(unix windows macosx))

(define empty-pkgdef
  (pkgdef-state "default"
                "default"
                "default"
                0
                null
                null
                (hasheq)
                null
                void
                null
                null
                ""
                ""
                ALL_OS_SYMS))


(begin-for-syntax
  (define-syntax-class os-sym
    (pattern (~var s id)
             #:when
             (member (syntax-e #'s)
                     '(unix windows macosx))))
  (define-syntax-class mod
    (pattern (~var m (static modifier? "package definition modifier"))
             #:with f (modifier-f (attribute m.value)))))


(define-syntax (expand-instance stx)
  (syntax-parse stx
    [(_) #'empty-pkgdef]
    [(_ (m:mod . args) . more)
     #'((m.f args) (expand-instance . more))]))


(define-syntax (#%module-begin* stx)
  (syntax-case stx ()
    [(_ . body)
     #'(collect-terms body () ())]))


(define-syntax (collect-terms stx)
  (syntax-parse stx
    [(_ () pd (ml ...))
     #'(#%module-begin
        ml ...
        (define pkgdef (expand-instance . pd))
        (define+provide-field provider pkgdef)
        (define+provide-field package pkgdef)
        (define+provide-field edition pkgdef)
        (define+provide-field revision-number pkgdef)
        (define+provide-field revision-names pkgdef)
        (define+provide-field racket-versions pkgdef)
        (define+provide-field metadata pkgdef)
        (define+provide-field inputs pkgdef)
        (define+provide-field build pkgdef)
        (define+provide-field output-names pkgdef)
        (define+provide-field tags pkgdef)
        (define+provide-field description pkgdef)
        (define+provide-field url pkgdef)
        (define+provide-field os-support pkgdef))]
    [(_ (x . xs) pd ml)
     (syntax-parse #'x
       #:literals (define)
       [(m:mod . args)
        #'(collect-terms xs (x . pd) ml)]
       [(define . args)
        #'(collect-terms xs pd (x . ml))])]))


;-------------------------------------------------------------------------------
; Controls how program terms impact instance of structure

(define-simple-macro (set-field st:expr f:id v:expr)
  (struct-copy pkgdef-state st [f v]))

(define-simple-macro (set-field* f:id v:expr)
  (λ (st) (set-field st f v)))

(define-syntax (define+provide-field stx)
  (syntax-parse stx
    [(_ field:id v:expr)
     (with-syntax ([acc (format-id stx "pkgdef-state-~a" #'field)])
       #'(begin (provide field)
                (define field (acc v))))]))

(begin-for-syntax (define-struct modifier (f)))

(define-simple-macro (define-modifier (m . args) body)
  (begin
    (define-syntax m-f
      (syntax-parser [(_ args) #'body]))
  (define-syntax m (modifier #'m-f))))


;-------------------------------------------------------------------------------
; Program terms

(define-modifier (provider name:non-empty-string)
  (set-field* provider name))

(define-modifier (package name:non-empty-string)
  (set-field* package name))

(define-modifier (edition name:non-empty-string)
  (set-field* edition name))

(define-modifier (revision-names name:non-empty-string ...)
  (set-field* revision-names '(name ...)))

(define-modifier (racket-versions ver:racket-version-selection ...+)
  (set-field* racket-versions '(ver ...)))

(define-modifier (revision-number num:exact-nonnegative-integer)
  (set-field* revision-number num))

(define-modifier (metadatum name:id v:string)
  (λ (st)
    (set-field st metadata
               (hash-set (pkgdef-state-metadata st) 'name v))))

(define-modifier (input name:non-empty-string . args)
  (λ (st) (set-field st inputs
                     (cons (make-input-info name . args) (pkgdef-state-inputs st)))))

(define-modifier (output name:non-empty-string steps:expr ...+)
  (λ (st)
    (set-field (set-field st output-names (cons name (pkgdef-state-output-names st)))
               build
               (λ (requested-name wd inputs rc messages)
                 (if (equal? requested-name name)
                     (call-with-applied-settings rc
                      (λ ()
                        (parameterize ([current-inputs inputs] [current-directory wd])
                          (run-log (mdo steps ...) messages))))
                     ((pkgdef-state-build st) requested-name wd inputs rc messages))))))

(define-modifier (os-support os:os-sym ...+)
  (set-field* os-support '(os ...)))

(define-modifier (tags t:non-empty-string ...)
  (set-field* tags '(t ...)))

(define-modifier (description desc:non-empty-string ...+)
  (set-field* description (string-join '(desc ...) "")))

(define-modifier (url u:url-string)
  (set-field* url u))


(module+ test
  (require racket/contract rackunit)
  (test-case "Expand package definition state"
    (define pkgdef
      (expand-instance
      (package "pkg")
      (edition "ed")
      (revision-number 9)
      (revision-names "n" "i" "c" "e")
      (provider "example.com")
      (input "archive")
      (racket-versions ("7.9" "*") "6.2")
      (metadatum boo "1")
      (output "default" (logged-unit 1))
      (input "int" '("src") (integrity 'sha1 (hex "abcd")))
      (metadatum foo "2")
      (tags "testable" "battle-ready")
      (description "A " "combined str" "ing")
      (input "sig"
             '("src")
             (integrity 'sha1 (hex "abcd"))
             (signature "pub" #"bytes"))
      (output "min" (logged-unit 2))
      (os-support windows macosx)
      (url "https://example.com/url")))
    (check-match
     pkgdef
     (pkgdef-state
      "example.com"
      "pkg"
      "ed"
      9
      '("n" "i" "c" "e")
      '(("7.9" "*") "6.2")
      (hash-table ('boo "1") ('foo "2"))
      (list (input-info "archive" (list) #f #f)
            (input-info "int"
                        '("src")
                        (integrity-info 'sha1 #"\253\315")
                        #f)
            (input-info "sig"
                        '("src")
                        (integrity-info 'sha1 #"\253\315")
                        (signature-info "pub" #"bytes")))
      (? procedure? _)
      '("default" "min")
      '("testable" "battle-ready")
      "A combined string"
      "https://example.com/url"
      '(windows macosx)))

    (define build (pkgdef-state-build pkgdef))

    (call-with-values (λ () (build "default" "." null null null))
                      (λ (v msg)
                        (check-equal? v 1)
                        (check-pred null? msg)))

    (call-with-values (λ () (build "min" "." null null null))
                      (λ (v msg)
                        (check-equal? v 2)
                        (check-pred null? msg)))))
