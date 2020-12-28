#lang racket/base

; Package definition expansion rules

(provide expand-pkgdef-module
         (struct-out pkgdef-state))

(require (only-in version/utils
                  valid-version?
                  version<=?)
         syntax/parse
         syntax/strip-context
         (only-in "../racket-version.rkt"
                  racket-version-selection)
         (only-in "../string.rkt"
                  non-empty-string
                  non-empty-string?
                  string-join)
         (only-in "../url.rkt"
                  url-string))

;-------------------------------------------------------------------------------
; Captures expansion state

(struct pkgdef-state
  (provider
   package
   edition
   revision-number
   revision-names
   racket-versions
   metadata
   inputs
   outputs
   actions
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
                null
                null
                null
                null
                null
                ""
                ""
                ALL_OS_SYMS))


(define-syntax-rule (set-field st field val)
  (struct-copy pkgdef-state st [field val]))

(define-syntax-class os-sym
  (pattern (~var s id)
           #:when
           (member (syntax-e #'s)
                   '(unix windows macosx))))


(define (update state stx)
  (syntax-parse stx
    #:datum-literals
    (action
     description
     edition
     url
     input
     metadatum
     output
     os-support
     package
     provider
     racket-versions
     revision-names
     revision-number
     tags)

    [(provider name:non-empty-string)
     (set-field state provider (syntax-e #'name))]

    [(package name:non-empty-string)
     (set-field state package (syntax-e #'name))]

    [(edition name:non-empty-string)
     (set-field state edition (syntax-e #'name))]

    [(revision-names name:non-empty-string ...)
     (set-field state revision-names (syntax->datum #'(name ...)))]

    [(racket-versions version:racket-version-selection ...+)
     (set-field state racket-versions (syntax->datum #'(version ...)))]

    [(revision-number num:exact-nonnegative-integer)
     (set-field state revision-number (syntax-e #'num))]

    [(metadatum name:id v:expr)
     (set-field state metadata (cons (cons #'name #'v) (pkgdef-state-metadata state)))]

    [(input name:non-empty-string _ ...)
     (set-field state inputs (cons stx (pkgdef-state-inputs state)))]

    [(output name:non-empty-string steps:expr ...+)
     (set-field state outputs (cons (cons #'name #'(steps ...)) (pkgdef-state-outputs state)))]

    [(os-support os:os-sym ...+)
     (set-field state os-support (syntax->datum #'(os ...)))]
    
    [(tags tag:non-empty-string ...)
     (set-field state tags (syntax->datum #'(tag ...)))]

    [(description desc:non-empty-string ...+)
     (set-field state description (string-join (syntax->datum #'(desc ...)) ""))]

    [(url u:url-string)
     (set-field state url (syntax-e #'u))]
    
    [(action (id:id sig:id ...+) body:expr ...+)
     (set-field state actions (cons stx (pkgdef-state-actions state)))]))


(define (expand-pkgdef-module-body state)
  (with-syntax ([provider/patt (pkgdef-state-provider state)]
                [package/patt (pkgdef-state-package state)]
                [edition/patt (pkgdef-state-edition state)]
                [url/patt (pkgdef-state-url state)]
                [revision-number/patt (pkgdef-state-revision-number state)]
                [(revision-names/patt ...) (pkgdef-state-revision-names state)]
                [(os ...) (pkgdef-state-os-support state)]
                [(tag ...) (pkgdef-state-tags state)]
                [([metadata-key . metadata-val] ...) (pkgdef-state-metadata state)]
                [([output-name . output-steps] ...) (pkgdef-state-outputs state)]
                [([_ (action-sig ...) action-steps ...] ...) (pkgdef-state-actions state)]
                [([_ (action-id _ ...) _ ...] ...) (pkgdef-state-actions state)]
                [(input-expr ...) (pkgdef-state-inputs state)])
    #'(begin (provide (except-out (all-defined-out) action-id ...))
             (define provider provider/patt)
             (define package package/patt)
             (define edition edition/patt)
             (define revision-number revision-number/patt)
             (define revision-names '(revision-names/patt ...))
             (define inputs (list input-expr ...))
             (define url url/patt)
             (define tags '(tag ...))
             (define os-support '(os ...))
             (define output-names '(output-name ...))
             (begin (define (action-sig ...) (do action-steps ...)) ...)
             (define (build target)
               (case target
                 [(output-name) (do . output-steps)]
                 ...
                 [else #f]))
             (define metadata (make-immutable-hasheq '((metadata-key . metadata-val) ...))))))

(define (expand-pkgdef-module data stx [state empty-pkgdef])
  (syntax-case data ()
    [(head . ())
     (replace-context stx (expand-pkgdef-module-body (update state #'head)))]
    [(head . tail)
     (expand-pkgdef-module #'tail stx (update state #'head))]))
