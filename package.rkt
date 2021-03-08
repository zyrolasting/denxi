#lang racket/base

; Define packages as active instantiations of package definitions.
; Interact with packages using sandboxed evaluators.
;
; This module follows a different style due to its complexity.
;
; - The comments double as table of contents for following code.
; - Multiple test submodules are allowed.
;

(provide install
         empty-package
         output-not-found
         (struct-out package))

(require racket/format
         racket/list
         "input-info.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "logged.rkt"
         "message.rkt"
         "monad.rkt"
         "pkgdef/static.rkt"
         "plugin.rkt"
         "port.rkt"
         "query.rkt"
         "racket-module.rkt"
         "racket-version.rkt"
         "source.rkt"
         "strict-rc.rkt"
         "string.rkt"
         "system.rkt"
         "url.rkt"
         "workspace.rkt")

(module+ test
  (require racket/function
           rackunit
           (submod "file.rkt" test)
           (submod "logged.rkt" test)
           (submod "plugin.rkt" test)
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

(define (output-not-found name)
  (logged-failure ($package:output:undefined)))

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

(define (install link-path-or-#f output-name-or-#f package-definition-source)
  (mdo pkg := (get-package #:override-specs (rc-ref 'XIDEN_INPUT_OVERRIDES)
                           #:before-new-package (load-plugin-override)
                           package-definition-source
                           (mebibytes->bytes (rc-ref 'XIDEN_FETCH_PKGDEF_SIZE_MB)))

       (fulfil-package-output #:allow-unsupported-racket? (rc-ref 'XIDEN_ALLOW_UNSUPPORTED_RACKET)
                              (or output-name-or-#f DEFAULT_STRING)
                              (or link-path-or-#f (package-name pkg))
                              pkg)

       (logged-unit (void))))


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


; Use the module namespace to dynamically extend registered modules.
(define-namespace-anchor anchor)
(define module-namespace (namespace-anchor->namespace anchor))

(define (get-package #:before-new-package [before-new-package values]
                     #:override-specs [override-specs null]
                     source
                     max-size)
  (mdo original := (find-original-package-definition source max-size)
       (let ([overridden (override-package-definition original before-new-package override-specs)])
         ; The reader/expander guards in read-package-definition
         ; prevent arbitrary code from reaching here.
         (eval overridden module-namespace)
         (logged-unit (dynamic-require `',(cadr overridden) 'pkg)))))


(define (find-original-package-definition pkgdef-origin-variant max-size)
  (if (string? pkgdef-origin-variant)
      (mdo variant := (fetch-package-definition (~a pkgdef-origin-variant)
                                                (coerce-source pkgdef-origin-variant)
                                                max-size)
           (read-package-definition variant))
      (read-package-definition pkgdef-origin-variant)))


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


(define (override-package-definition datum before-new-package override-specs)
  (define stripped (strip datum))
  (define plugin-override (before-new-package stripped))
  (define package-name (get-static-abbreviated-query plugin-override))
  (define input-overrides (find-per-input-overrides package-name override-specs))
  (make-package-definition-datum #:id (make-id)
   (bare-racket-module-code
    (override-inputs plugin-override input-overrides))))


(define (make-id)
  (string->symbol
   (format
    "pkgdef~a~a"
    (current-seconds)
    (build-string 10
                  (λ _ (integer->char (+ (char->integer #\0)
                                         (random 0 10))))))))

(define (fetch-package-definition name source max-size)
  (logged-fetch name
                source
                (λ (from-source est-size)
                  (make-limited-input-port from-source
                                           (min max-size est-size)
                                           #f))))

(define (load-plugin-override)
  (load-from-plugin 'before-new-package
                    (λ () values)
                    raise))


(module+ test
  (test-case "Override package definitions"
    (define (before-new-package orig)
      (struct-copy bare-racket-module orig
                   [code (list-set (bare-racket-module-code orig)
                                   1
                                   '(package "tweedledum"))]))

    (define overridden
      (override-package-definition
       (make-package-definition-datum
        '((provider "wonderland")
          (package "tweedledee")
          (input "logic")
          (input "nonsense")))
       before-new-package
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
  (logged-combine (mdo (validate-requested-output pkg output-name) ; 3.1
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

(define-logged (validate-inputs pkg)
  (for ([input (in-list (package-inputs pkg))])
    (when (abstract-input-info/c input)
      ($fail ($package:abstract-input (input-info-name input)))))
  ($use pkg))


(define-logged (validate-os-support pkg)
  (let ([supported (package-os-support pkg)])
    (if (member (system-type 'os) supported)
        ($use pkg)
        ($fail ($package:unsupported-os supported)))))


(define-logged (validate-racket-support #:allow-unsupported? allow-unsupported? pkg)
  (let ([racket-support (check-racket-version-ranges (version) (package-racket-versions pkg))])
    (case racket-support
      [(supported undeclared) ($use pkg)]
      [(unsupported)
       (if allow-unsupported?
           ($use pkg)
           ($fail ($package:unsupported-racket-version racket-support)))])))


(define-logged (validate-requested-output pkg requested)
  (let ([available (package-output-names pkg)])
    (if (member requested available)
        ($use pkg)
        ($fail ($package:unavailable-output available)))))


(module+ test
  (let ([ev (build-package [output-names '("default")])])
    (test-logged-procedure
     "Allow only defined outputs"
     (validate-requested-output ev "default")
     (λ (val messages)
       (check-eq? val ev)
       (check-pred null? messages)))
    (test-logged-procedure
     "Reject unavailable outputs"
     (validate-requested-output ev "other")
     (λ (val messages)
       (check-eq? val FAILURE)
       (check-match messages
                    (list ($package:unavailable-output '("default")))))))

  (let ([ev (build-package [inputs (list (make-input-info "c1" (sources "s"))
                                         (make-input-info "c2" (sources "s2")))])])
    (test-logged-procedure
     "Allow all concrete inputs"
     (validate-inputs ev)
     (λ (val messages)
       (check-eq? val ev)
       (check-pred null? messages))))

  (test-logged-procedure
   "Disallow abstract inputs"
   (validate-inputs
    (build-package [inputs (list (make-input-info "concrete" (sources "s") (integrity 'sha1 #""))
                                 (make-input-info "a"))]))
   (λ (val messages)
     (check-eq? val FAILURE)
     (check-match messages
                  (list ($package:abstract-input "a")))))


  (let ([other-os (filter (λ (v) (not (eq? v (system-type 'os)))) ALL_OS_SYMS)])
    (test-logged-procedure
     "Disallow packages that don't list current os support"
     (validate-os-support (build-package [os-support other-os]))
     (λ (val messages)
       (check-eq? val FAILURE)
       (check-match messages
                    (list ($package:unsupported-os (? (λ (v) (equal? v other-os)) _)))))))

  (let ([ev (build-package [os-support (list (system-type 'os))])])
    (test-logged-procedure
     "Allow packages that support the current os"
     (validate-inputs ev)
     (λ (val messages)
       (check-eq? val ev)
       (check-pred null? messages))))

  (test-case "Check Racket version support"
    (define with-unsupported-version
      (build-package [racket-versions '("0.0")]))

    (test-logged-procedure
     "Detect packages that declare an unsupported Racket version"
     (validate-racket-support #:allow-unsupported? #f with-unsupported-version)
     (λ (val msg)
       (check-pred $package:unsupported-racket-version?
                   (car msg))))

    (test-logged-procedure
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
            (mdo directory-record := (build-package-output-directory pkg output-name)

                 (build-package-output pkg
                                       output-name
                                       (build-workspace-path (path-record-path directory-record)))

                 (record-package-output pkg
                                        output-name
                                        directory-record
                                        link-path))]))))

(define-logged (reuse-package-output pkg output-name output-record-inst link-path)
  ($attach (make-addressable-link (find-path-record (output-record-path-id output-record-inst)) link-path)
           ($package:output:reused)))

(define (build-package-output pkg output-name build-directory)
  (logged-acyclic
   (~a (abbreviate-exact-package-query (package->package-query pkg)) ", " output-name ", " build-directory)
   (λ (messages)
     (parameterize ([current-directory build-directory]
                    [current-inputs (package-inputs pkg)])
       (run-log ((package-build pkg) output-name)
                messages)))))

(define-logged (build-package-output-directory pkg output-name)
  ($attach
   (make-addressable-directory
    (cons (open-input-string output-name)
          (map open-input-info-as-bytes (package-inputs pkg))))))

(define (open-input-info-as-bytes info)
  (open-input-bytes
    (with-handlers ([values (λ (e) (string->bytes/utf-8 (input-info-name info)))])
      (integrity-info-digest (input-info-integrity info)))))

(define-logged (record-package-output pkg output-name directory-record link-path)
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
  (test-case "Stop cyclic package builds"
    (call-with-temporary-file
     (λ (tmp-file-path)
       (write-to-file #:exists 'truncate/replace
                      (make-package-definition-datum `((output "default" (install #f #f ,(~a tmp-file-path)))))
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
