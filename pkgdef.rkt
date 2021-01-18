#lang racket/base

; Module language for package definitions

(provide #%app
         #%datum
         :=
         base32
         base64
         coerce-source
         define
         description
         edition
         extract
         extract-input
         file-source
         find-input
         from-catalogs
         from-file
         getenv
         hex
         http-mirrors-source
         http-source
         in-paths
         input-ref
         input
         integrity
         install
         keep-input
         lines-source
         mdo
         metadatum
         name
         os-support
         output
         plugin-source
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
         text-source
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
                     (only-in "system.rkt"
                              ALL_OS_SYMS
                              os-sym)
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
         "package.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "source.rkt"
         "system.rkt")


;-------------------------------------------------------------------------------
; The module language collects terms to build an instance of package.

(define-syntax-rule (#%module-begin* . body)
  (collect-terms body () ()))

(define-simple-macro (set-field st:expr f:id v:expr)
  (struct-copy package st [f v]))

(define-simple-macro (set-field* f:id v:expr)
  (λ (st) (set-field st f v)))

(begin-for-syntax
  (define-struct modifier (f)
    #:property prop:procedure
    (λ (m stx)
      (syntax-parse stx
        [(_ . args)
         (with-syntax ([f (modifier-f m)])
           #'(f args))]))))


(define-syntax (define-modifier stx)
  (syntax-parse stx
    #:track-literals
    [((~var macro-id id) (m . args) body)
     (with-syntax ([new-id (format-id #'m "~a-parser" #'m)])
       #'(begin
           (define-syntax new-id
             (syntax-parser [(_ args) #'body]))
           (define-syntax m (modifier #'new-id))))]))


(begin-for-syntax
  (define-syntax-class mod
    (pattern (~var m (static modifier? "package definition modifier"))
             #:with f (modifier-f (attribute m.value)))))


(define-syntax (collect-terms stx)
  (syntax-parse stx
    [(_ () pd ml)
     #'(#%module-begin (begin . ml)
                       (provide pkg)
                       (define pkg (expand-instance . pd)))]
    [(_ (x . xs) pd ml)
     (syntax-parse #'x
       #:literals (define define-values #%expression)
       [(m:mod . args)
        #'(collect-terms xs (x . pd) ml)]
       [(define . args)
        #'(collect-terms xs pd (x . ml))]
       ; The below are artifacts of partial expansion in single-form modules.
       [(#%expression sub)
        #'(collect-terms xs (sub . pd) ml)]
       [(define-values . args)
        #'(collect-terms xs pd (x . ml))])]))



(define-syntax (expand-instance stx)
  (syntax-parse stx
    [(_) #'empty-package]
    [(_ (m:mod . args) . more)
     #'((m.f args) (expand-instance . more))]
    [(_ partial . more)
     #'(partial (expand-instance . more))]))



;-------------------------------------------------------------------------------
; Program terms

(define-modifier (description desc:non-empty-string ...+)
  (set-field* description (string-join '(desc ...) "")))

(define-modifier (edition name:non-empty-string)
  (set-field* edition name))

(define-modifier (input name:non-empty-string . args)
  (λ (st)
    (set-field st inputs
               (cons (make-input-info name . args) (package-inputs st)))))

(define-modifier (metadatum name:id v:string)
  (λ (st)
    (set-field st metadata
               (hash-set (package-metadata st) 'name v))))

(define-modifier (os-support os:os-sym ...+)
  (set-field* os-support '(os ...)))

(define-modifier (output name:non-empty-string steps:expr ...+)
  (λ (st)
    (set-field (set-field st output-names (cons name (package-output-names st)))
               build
               (λ (requested-name)
                 (if (equal? requested-name name)
                     (coerce-logged (mdo steps ...))
                     ((package-build st) requested-name))))))

(define-modifier (name n:non-empty-string)
  (set-field* name n))

(define-modifier (provider name:non-empty-string)
  (set-field* provider name))

(define-modifier (racket-versions ver:racket-version-selection ...+)
  (set-field* racket-versions '(ver ...)))

(define-modifier (revision-names name:non-empty-string ...)
  (set-field* revision-names '(name ...)))

(define-modifier (revision-number num:exact-nonnegative-integer)
  (set-field* revision-number num))

(define-modifier (tags t:non-empty-string ...)
  (set-field* tags '(t ...)))

(define-modifier (url u:url-string)
  (set-field* url u))


(module+ test
  (require racket/contract rackunit)

  (test-case "Expand package definition state"
    (define pkgdef
      (expand-instance
       (name "pkg")
       (edition "ed")
       (revision-number 9)
       (revision-names "n" "i" "c" "e")
       (provider "example.com")
       (input "archive")
       (racket-versions ("7.9" "*") "6.2")
       (metadatum boo "1")
       (output "default" 1)
       (input "int" (sources "src") (integrity 'sha1 (hex "abcd")))
       (metadatum foo "2")
       (tags "testable" "battle-ready")
       (description "A " "combined str" "ing")
       (input "sig"
              (sources "src")
              (integrity 'sha1 (hex "abcd"))
              (signature "pub" #"bytes"))
       (output "min" 2)
       (os-support windows macosx)
       (url "https://example.com/url")))
    (check-match
     pkgdef
     (package
      "A combined string"
      '("testable" "battle-ready")
      "https://example.com/url"
      "example.com"
      "pkg"
      "ed"
      9
      '("n" "i" "c" "e")
      '(windows macosx)
      '(("7.9" "*") "6.2")
      (hash-table ('boo "1") ('foo "2"))
      (list (input-info "archive" #f #f #f)
            (input-info "int"
                        (? source? _)
                        (integrity-info 'sha1 #"\253\315")
                        #f)
            (input-info "sig"
                        (? source? _)
                        (integrity-info 'sha1 #"\253\315")
                        (signature-info "pub" #"bytes")))
      '("default" "min")
      (? procedure? _)))

    (define build (package-build pkgdef))

    (define (check l e)
      (call-with-values (λ () (run-log l))
                        (λ (v m)
                          (check-equal? v e)
                          (check-pred null? m))))

    (check (build "default") 1)
    (check (build "min") 2)))
