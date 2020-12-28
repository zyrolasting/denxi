#lang racket/base

; Define packages as active instantiations of package definitions.
; Interact with packages using sandboxed evaluators.

(provide run-package)

(require racket/function
         racket/format
         version/utils
         "codec.rkt"
         "contract.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "logged.rkt"
         "message.rkt"
         "monad.rkt"
         "pkgdef/static.rkt"
         "path.rkt"
         "plugin.rkt"
         "port.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "sandbox.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "version.rkt"
         "workspace.rkt")

(define+provide-message $package ())
(define+provide-message $package:log (query output-name messages))
(define+provide-message $package:output $package ())
(define+provide-message $package:output:built $package:output ())
(define+provide-message $package:output:reused $package:output ())
(define+provide-message $package:output:undefined $package:output ())
(define+provide-message $package:abstract-input $package (versions))
(define+provide-message $package:unsupported-racket-version $package (versions))
(define+provide-message $package:unsupported-os $package (supported))

(define DEFAULT_OUTPUT "default")


;----------------------------------------------------------------------------
; High-level interface

(define (run-package pkg-definition-variant
                     #:output-name [output-name DEFAULT_OUTPUT]
                     #:link-path [link-path #f])
  (do pkgeval <- (make-package pkg-definition-variant)
      (let ([package-name (abbreviate-exact-xiden-query (package-evaluator->xiden-query pkgeval))])
        (logged-combine (do (install-output! pkgeval output-name (or link-path (pkgeval 'package))
                                             (return (logged-unit (kill-evaluator pkgeval))))
                            (λ (to-wrap messages)
                              (cons ($package:log package-name output-name to-wrap)
                                    messages)))))))



;----------------------------------------------------------------------------
; Validation is about making sure the package and its deliverables can
; run in the current environment.

(define-logged (validate-inputs pkgeval)
  (for ([input (in-list (pkgeval 'inputs))])
    (when (abstract-input-info/c input)
      ($fail ($package:abstract-input (input-info-name input)))))
  ($use pkgeval))


(define-logged (validate-os-support pkgeval)
  (let ([supported (pkgeval 'os-support)])
    (if (member (system-type 'os) supported)
        ($use pkgeval)
        ($fail ($package:unsupported-os supported)))))


(define-logged (validate-racket-support pkgeval)
  (let ([racket-support (check-racket-version-ranges (version) (pkgeval 'racket-versions))])
    (case racket-support
      [(supported) ($use pkgeval)]
      [(unsupported)
       (if (XIDEN_ALLOW_UNSUPPORTED_RACKET)
           ($use pkgeval)
           ($fail ($package:unsupported-racket-version racket-support)))])))


;----------------------------------------------------------------------------
; Output installation
;
; When installing a package output, we can either reuse existing
; output files or build a new distribution of output files.

(define (install-output! pkgeval output-name link-path)
  (call-with-reused-output
   (package-evaluator->xiden-query pkgeval)
   output-name
   (λ (variant)
     (cond [(exn? variant)
            (raise variant)]
           [(output-record? variant)
            (reuse-package-output pkgeval output-name variant link-path)]
           [else
            (do directory-record <- (build-package-output pkgeval output-name link-path)
                (record-package-output pkgeval output-name directory-record link-path))]))))


(define-logged (reuse-package-output pkgeval output-name output-record-inst link-path)
  (make-addressable-link (find-path-record (output-record-path-id output-record-inst))
                         link-path)
  ($pass ($package:output:reused)))

(define-logged (build-package-output pkgeval output-name output-record-inst link-path)
  (make-addressable-directory
   (cons (open-input-string output-name)
         (map open-input-info-as-bytes
              (xiden-evaluator-ref pkgeval 'inputs null)))
   (λ (build-directory)
     (define variant (pkgeval `(build ,output-name)))
     (define program (if (logged? variant) variant (logged-unit variant)))
     (if variant
         (parameterize ([current-directory build-directory]) ($run! program))
         ($fail ($package:output:undefined))))))

(define-logged (record-package-output pkgeval output-name directory-record link-path)
  (declare-output (pkgeval 'provider)
                  (pkgeval 'package)
                  (pkgeval 'edition)
                  (pkgeval 'revision-number)
                  (pkgeval 'revision-names)
                  output-name
                  directory-record)
  (make-addressable-link directory-record link-path)
  ($pass ($package:output:built)))


(define (open-input-info-as-bytes info)
  (open-input-bytes
    (with-handlers ([values (λ (e) (string->bytes/utf-8 (input-info-name info)))])
      (integrity-info-digest (input-info-integrity info)))))


(define (package-evaluator->xiden-query pkgeval)
  (xiden-query (pkgeval 'provider)
               (pkgeval 'package)
               (pkgeval 'edition)
               (~a (pkgeval 'revision-number))
               (~a (pkgeval 'revision-number))
               "ii"))


;----------------------------------------------------------------------------
; Package operations

(define (make-package source)
  (do sourced-eval <- (if (string? source)
                          (fetch-package-definition source)
                          (logged-unit (load-xiden-module (override-package-definition source))))
      (validate-inputs sourced-eval)
      (validate-os-support sourced-eval)
      (validate-racket-support sourced-eval)
      (return sourced-eval)))


(define (override-package-definition variant)
  (define plugin-override (load-from-plugin 'before-new-package (λ () values) (λ (e) values)))
  (plugin-override (read-package-definition variant)))


; This is the inflection point between restricted and unrestricted
; resources for an evaluator.
(define (call-with-build-sandbox-parameterization proc)
  (parameterize ([sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-security-guard
                  (make-security-guard
                   (current-security-guard)
                   (make-pkgeval-file-guard (XIDEN_TRUSTED_EXECUTABLES)
                                            (list (build-workspace-path "var/xiden")
                                                  (build-workspace-path "tmp")))
                   (make-pkgeval-network-guard)
                   (make-pkgeval-link-guard (workspace-directory)))]
                 [sandbox-make-environment-variables
                  (bind-envvar-subset (cons "PATH" (XIDEN_ALLOW_ENV)))]
                 [sandbox-namespace-specs
                  (append (sandbox-namespace-specs)
                          '(xiden/rc xiden/package))])
    (proc)))


(define (make-pkgeval-file-guard allowed-executables write-dirs)
  (define (check-destructive-op op path)
    (define test (curry path-prefix? (normalize-path path)))
    (unless (ormap test write-dirs)
      (raise-user-error (format "Unauthorized attempt to ~a in ~a" op path))))

  (define trust-executable?
    (bind-trust-list allowed-executables))

  (λ (sym path-or-#f ops)
    (when path-or-#f
      (cond [(member 'execute ops)
             (unless (or (equal? "openssl" (path->string (file-name-from-path path-or-#f)))
                         (XIDEN_TRUST_ANY_EXECUTABLE)
                         (trust-executable? path-or-#f))
               (raise-user-error 'security
                                 (~a "Unauthorized attempt to execute ~a.~n"
                                     "To trust this executable, add this to ~a:~n"
                                     "(integrity 'sha384 (hex ~s))")
                                 path-or-#f
                                 (setting-id XIDEN_TRUSTED_EXECUTABLES)
                                 (~a (encode 'hex (make-digest path-or-#f 'sha384)))))]

            [(member 'write ops)
             (check-destructive-op "write" path-or-#f)]

            [(member 'delete ops)
             (check-destructive-op "delete" path-or-#f)]))))


(define (make-pkgeval-network-guard)
  (λ (sym hostname-or-#f port-or-#f client-or-server)
    (unless hostname-or-#f
      (raise-user-error 'security
                        "Unauthorized attempt to listen for connections"))))


(define (make-pkgeval-link-guard workspace)
  (define (path-ok? p)
    (path-prefix? (simplify-path (if (complete-path? p) p (build-path workspace p)))
                  workspace))

  (λ (op link-path target-path)
    (unless (path-ok? (normalize-path target-path))
      (raise-user-error 'security
                        "Cannot create link. Target must be in ~a~n  target path: ~a"
                        workspace
                        target-path))))


(define-logged (fetch-package-definition source)
  (define logged/fetch-st
    (fetch source
           (list source)
           (λ (from-source est-size)
             (call-with-build-sandbox-parameterization
              (λ ()
                (load-xiden-module
                 (override-package-definition
                  (make-limited-input-port from-source
                                           (min (mebibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB))
                                                est-size)
                                           #t))))))))

  (define-values (fetch-st messages) ($run! logged/fetch-st))
  (if (and (fetch-state? fetch-st) (fetch-state-result fetch-st))
      ($use (fetch-state-result fetch-st) messages)
      ($use FAILURE messages)))


(module+ test
  (require racket/runtime-path
           rackunit
           (submod "file.rkt" test)
           "setting.rkt")

  (test-case "Check Racket version support"
    (define (make-dummy-pkginfo versions)
      `(module anon ,PACKAGE_DEFINITION_MODULE_LANG
         (racket-versions . ,versions)))

    (define (test-case/racket-version msg versions instance?)
      (test-case "Detect packages that do not declare a supported Racket version"
        (define pkginfo (make-dummy-pkginfo versions))
        (define-values (_ messages) (run-log (validate-racket-support pkginfo)))
        (check-pred instance? (car messages))))

    (test-case/racket-version
     "Detect packages that declare an unsupported Racket version"
     '("0.0")
     $package:unsupported-racket-version?)))
