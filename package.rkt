#lang racket/base

; Define packages as active instantiations of package definitions.
; Interact with packages using sandboxed evaluators.
;
; This module follows a different style due to its complexity.
;
; - The comments double as table of contents for following code.
; - Multiple test submodules are allowed.
;

(require racket/contract)
(provide (struct-out package)
         (contract-out
          [install
           (-> (or/c #f path-string?)
               (or/c #f string?)
               any/c
               subprogram?)]
          [output-not-found
           (-> string? subprogram?)]
          [empty-package
           package?]
          [sxs
           (-> package? (subprogram/c package?))]
          [load-user-package-definition
           (-> source-variant?
               (subprogram/c package-definition-datum?))]
          [build-user-package
           (-> package-definition-datum?
               (subprogram/c package?))]
          [current-package-definition-editor
           (parameter/c
            (-> bare-racket-module?
                (or/c bare-racket-module? (subprogram/c bare-racket-module?))))]
          [current-package-editor
           (parameter/c
            (-> package?
                (or/c package? (subprogram/c package?))))]))

(require racket/file
         racket/format
         racket/list
         racket/random
         "artifact.rkt"
         "codec.rkt"
         "input.rkt"
         "integrity.rkt"
         "state.rkt"
         "message.rkt"
         "monad.rkt"
         "pkgdef/static.rkt"
         "port.rkt"
         "query.rkt"
         "racket-module.rkt"
         "racket-version.rkt"
         "setting.rkt"
         "source.rkt"
         "string.rkt"
         "subprogram.rkt"
         "system.rkt"
         "url.rkt")

(module+ test
  (require racket/function
           rackunit
           (submod "state.rkt" test)
           (submod "subprogram.rkt" test)
           "file.rkt"
           "setting.rkt"))


(define+provide-message $package ())
(define+provide-message $package:log $package (query output-name messages))
(define+provide-message $package:output $package ())
(define+provide-message $package:output:built $package:output ())
(define+provide-message $package:output:reused $package:output ())
(define+provide-message $package:output:undefined $package:output ())
(define+provide-message $package:abstract-input $package (versions))
(define+provide-message $package:unsupported-racket-version $package (versions))
(define+provide-message $package:unsupported-os $package (supported))
(define+provide-message $package:unavailable-output $package (available))

(define+provide-setting XIDEN_INSTALL_ABBREVIATED_SOURCES (listof string?) null)
(define+provide-setting XIDEN_INSTALL_DEFAULT_SOURCES (listof (list/c string? string?)) null)
(define+provide-setting XIDEN_INSTALL_SOURCES (listof (list/c string? string? string?)) null)
(define+provide-setting XIDEN_ALLOW_UNSUPPORTED_RACKET boolean? #f)
(define+provide-setting XIDEN_INPUT_OVERRIDES
  (listof (list/c (or/c symbol? string? regexp? pregexp? byte-regexp? byte-pregexp?) list?))
  null)

(define current-package-definition-editor
  (make-parameter values))

(define current-package-editor
  (make-parameter values))

(struct package
  (description
   tags
   url
   provider
   name
   edition
   revision-number
   revision-names
   os-support
   racket-versions
   metadata
   inputs
   output-names
   build))


(define (sxs pkg)
  (subprogram
   (λ (messages)
     (define new-provider
       (string-replace
        (coerce-string (encode 'base32 (crypto-random-bytes 32)))
        "=" ""))
     (values (struct-copy package pkg [provider new-provider])
             (cons ($show-string (~a "sxs: " (package-provider pkg) " ~> " new-provider))
                   messages)))))


(define (output-not-found name)
  (subprogram-failure ($package:output:undefined)))


(define empty-package
  (package ""
           null
           ""
           DEFAULT_STRING
           DEFAULT_STRING
           DEFAULT_STRING
           0
           null
           ALL_OS_SYMS
           '(("*" "*"))
           (hasheq)
           null
           null
           output-not-found))

(define-syntax-rule (build-package fields ...)
  (struct-copy package empty-package fields ...))


(define (install link-path-or-#f output-name-or-#f pkgdef-source)
  (mdo pkgdef := (load-user-package-definition pkgdef-source)
       pkg := (build-user-package pkgdef)

       (fulfil-package-output #:allow-unsupported-racket? (XIDEN_ALLOW_UNSUPPORTED_RACKET)
                              (or output-name-or-#f DEFAULT_STRING)
                              (or link-path-or-#f (package-name pkg))
                              pkg)

       (subprogram-unit (void))))


(module+ test
  (test-case "Force SxS installations"
    (define (sample)
      (package-provider (get-subprogram-value (sxs empty-package))))
    (check-not-equal? (sample) (sample))))



;===============================================================================
; 1: MAKE PACKAGE
;
; The user provides a package definition source. If it's a string,
; treat it like a source for a package input. Otherwise, try to use
; the source as an input program.
;
; The RC may override the definition to resolve abstract inputs,
; standardize dependencies, etc.
;


(define (load-user-package-definition source)
  (define max-size (mebibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB)))
  (define override-specs (XIDEN_INPUT_OVERRIDES))
  (mdo original-package-definition :=
       (find-original-package-definition source max-size)

       stripped-package-definition :=
       (subprogram-unit (strip original-package-definition))

       user-modified-package-definition :=
       (coerce-subprogram ((current-package-definition-editor) stripped-package-definition))

       (subprogram-unit
        (override-package-definition user-modified-package-definition
                                     override-specs))))


; Use the module namespace to dynamically extend registered modules.
(define-namespace-anchor anchor)
(define module-namespace (namespace-anchor->namespace anchor))

(define (build-user-package pkgdef)
  (mdo evaluated-package :=
       (subprogram
        (λ (messages)
          (eval pkgdef module-namespace)
          (values (dynamic-require `',(cadr pkgdef) 'pkg)
                  messages)))
       (coerce-subprogram ((current-package-editor)
                           evaluated-package))))


(define (find-original-package-definition pkgdef-origin-variant max-size)
  (mdo variant := (fetch-package-definition (~a pkgdef-origin-variant)
                                            (coerce-source pkgdef-origin-variant)
                                            max-size)
       (read-package-definition variant)))


(define (find-per-input-overrides package-name override-specs)
  (filter-map (λ (spec)
                (define pattern-variant
                  (car spec))

                (define pattern
                  (cond [(string? pattern-variant)
                         (pregexp pattern-variant)]
                        [(symbol? pattern-variant)
                         (pregexp (~a pattern-variant))]
                        [else pattern-variant]))

                (and (regexp-match? pattern package-name)
                     (cadr spec)))
              override-specs))


(define (override-package-definition code-override override-specs)
  (define package-name (get-static-abbreviated-query code-override))
  (define input-overrides (find-per-input-overrides package-name override-specs))
  (make-package-definition-datum #:id (make-id)
                                 (bare-racket-module-code
                                  (override-inputs code-override input-overrides))))


(define (make-id)
  (string->symbol
   (format
    "pkgdef~a~a"
    (current-seconds)
    (build-string 10
                  (λ _ (integer->char (+ (char->integer #\0)
                                         (random 0 10))))))))

(define (fetch-package-definition name source max-size)
  (subprogram-fetch name
                    source
                    (λ (from-source est-size)
                      (make-limited-input-port from-source
                                               (min max-size est-size)
                                               #f))))



(module+ test
  (test-case "Override package definitions"
    (define overridden
      (override-package-definition
       (strip
        (make-package-definition-datum
        '((provider "wonderland")
          (package "tweedledum")
          (input "logic")
          (input "nonsense"))))
       '((#px"dum" (input "logic" (integrity 'sha1 (hex "abc"))))
         (#px"zooks" (input "nonsense" (integrity 'sha1 (hex "abc")))))))

    (check-match overridden
                 `(module ,_ ,(? (curry equal? PACKAGE_DEFINITION_MODULE_LANG) _)
                    (provider "wonderland")
                    (package "tweedledum")
                    (input "logic" (integrity 'sha1 (hex "abc")))
                    (input "nonsense")))))



;===============================================================================
; 3: OUTPUT FULFILMENT

(define (fulfil-package-output #:allow-unsupported-racket? allow-unsupported-racket?
                               output-name
                               link-path
                               pkg)
  (subprogram-combine (mdo (validate-requested-output pkg output-name) ; 3.1
                           (validate-inputs pkg)
                           (validate-os-support pkg)
                           (validate-racket-support #:allow-unsupported? allow-unsupported-racket? pkg)
                           (reuse-or-build-package-output pkg output-name link-path)) ; 3.2
                      (λ (to-wrap messages)
                        (cons ($package:log (abbreviate-exact-package-query (package->package-query pkg))
                                            output-name
                                            (reverse to-wrap))
                              messages))))

;-------------------------------------------------------------------------------
; 3.1: Validation
;
; Makes sure that the package agrees with the environment and runtime
; configuration.

(define-subprogram (validate-inputs pkg)
  (for ([input (in-list (package-inputs pkg))])
    (when (abstract-package-input? input)
      ($fail ($package:abstract-input (package-input-name input)))))
  ($use pkg))


(define-subprogram (validate-os-support pkg)
  (let ([supported (package-os-support pkg)])
    (if (member (system-type 'os) supported)
        ($use pkg)
        ($fail ($package:unsupported-os supported)))))


(define-subprogram (validate-racket-support #:allow-unsupported? allow-unsupported? pkg)
  (let ([racket-support (check-racket-version-ranges (version) (package-racket-versions pkg))])
    (case racket-support
      [(supported undeclared) ($use pkg)]
      [(unsupported)
       (if allow-unsupported?
           ($use pkg)
           ($fail ($package:unsupported-racket-version racket-support)))])))


(define-subprogram (validate-requested-output pkg requested)
  (let ([available (package-output-names pkg)])
    (if (member requested available)
        ($use pkg)
        ($fail ($package:unavailable-output available)))))


(module+ test
  (let ([ev (build-package [output-names (list DEFAULT_STRING)])])
    (test-subprogram
     "Allow only defined outputs"
     (validate-requested-output ev DEFAULT_STRING)
     (λ (val messages)
       (check-eq? val ev)
       (check-pred null? messages)))
    (test-subprogram
     "Reject unavailable outputs"
     (validate-requested-output ev "other")
     (λ (val messages)
       (check-eq? val FAILURE)
       (check-match messages
                    (list ($package:unavailable-output (list DEFAULT_STRING)))))))

  (let ([ev (build-package
             [inputs (list (package-input "c1"
                                          (artifact (sources "s")))
                           (package-input "c2"
                                          (artifact (sources "s2"))))])])
    (test-subprogram
     "Allow all concrete inputs"
     (validate-inputs ev)
     (λ (val messages)
       (check-eq? val ev)
       (check-pred null? messages))))

  (test-subprogram
   "Disallow abstract inputs"
   (validate-inputs
    (build-package [inputs (list (package-input "concrete"
                                                (artifact
                                                 (sources "s")
                                                 (integrity 'sha1 #"")))
                                 (make-package-input "a"))]))
   (λ (val messages)
     (check-eq? val FAILURE)
     (check-match messages
                  (list ($package:abstract-input "a")))))


  (let ([other-os (filter (λ (v) (not (eq? v (system-type 'os)))) ALL_OS_SYMS)])
    (test-subprogram
     "Disallow packages that don't list current os support"
     (validate-os-support (build-package [os-support other-os]))
     (λ (val messages)
       (check-eq? val FAILURE)
       (check-match messages
                    (list ($package:unsupported-os (? (λ (v) (equal? v other-os)) _)))))))

  (let ([ev (build-package [os-support (list (system-type 'os))])])
    (test-subprogram
     "Allow packages that support the current os"
     (validate-inputs ev)
     (λ (val messages)
       (check-eq? val ev)
       (check-pred null? messages))))

  (test-case "Check Racket version support"
    (define with-unsupported-version
      (build-package [racket-versions '("0.0")]))

    (test-subprogram
     "Detect packages that declare an unsupported Racket version"
     (validate-racket-support #:allow-unsupported? #f with-unsupported-version)
     (λ (val msg)
       (check-pred $package:unsupported-racket-version?
                   (car msg))))

    (test-subprogram
     "Conditionally allow unsupported Racket versions"
     (validate-racket-support #:allow-unsupported? #t with-unsupported-version)
     (λ (val msg) (check-pred null? msg)))))



;-------------------------------------------------------------------------------
; 3.2: Creating or reusing output
;
; When installing a package output, we can either reuse existing
; output files or build a new distribution of output files.

(define (reuse-or-build-package-output pkg output-name link-path)
  (call-with-reused-output
   (package->package-query pkg)
   output-name
   (λ (variant)
     (cond [(exn? variant)
            (raise variant)]
           [(output-record? variant)
            (reuse-package-output pkg output-name variant link-path)]
           [else
            (mdo temp-directory := (build-package-output pkg output-name)
                 directory-record := (subprogram-unit (make-addressable-directory temp-directory))
                 (record-package-output pkg
                                        output-name
                                        directory-record
                                        link-path))]))))

(define-subprogram (reuse-package-output pkg output-name output-record-inst link-path)
  ($attach (make-addressable-link (find-path-record (output-record-path-id output-record-inst)) link-path)
           ($package:output:reused)))

(define (build-package-output pkg output-name)
  (subprogram-acyclic
   (~a (abbreviate-exact-package-query (package->package-query pkg)) ", " output-name)
   (λ (messages)
     (define tmp
       (make-temporary-file "~a"
                            'directory
                            (build-object-path)))
     (define-values (result messages*)
       (parameterize ([current-directory tmp]
                      [current-inputs (package-inputs pkg)])
         (run-subprogram ((package-build pkg) output-name)
                         messages)))

     (values (if (eq? result FAILURE) FAILURE tmp)
             messages*))))

(define-subprogram (record-package-output pkg output-name directory-record link-path)
  (declare-output (package-provider pkg)
                  (package-name pkg)
                  (package-edition pkg)
                  (package-revision-number pkg)
                  (package-revision-names pkg)
                  output-name
                  directory-record)
  (make-addressable-link directory-record link-path)
  ($use (void)))



(module+ test
  (test-workspace "Stop cyclic package builds"
                  (call-with-temporary-file
                   (λ (tmp-file-path)
                     (write-to-file #:exists 'truncate/replace
                                    (make-package-definition-datum
                                     `((output ,DEFAULT_STRING
                                               (install #f #f ,(~a tmp-file-path)))))
                                    tmp-file-path)
                     (call-with-values (install #f #f (~a tmp-file-path))
                                       (λ (v m)
                                         (check-eq? v FAILURE)
                                         (check-match
                                          (car m)
                                          ($package:log _ _
                                                        (list ($fetch _ _)
                                                              ($package:log _ _ (list ($cycle _))))))))))))



;===============================================================================
; A: Supporting procedures

(define (package->package-query pkg)
  (make-exact-package-query
   (package-provider pkg)
   (package-name pkg)
   (package-edition pkg)
   (package-revision-number pkg)))
