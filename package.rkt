#lang racket/base

(provide (all-defined-out))

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
         "package-definition.rkt"
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

(define+provide-message $no-package-info (source))
(define+provide-message $output-not-found (query output-name))

(define+provide-message $package ())
(define+provide-message $package:log (query output-name messages))
(define+provide-message $package:output $package ())
(define+provide-message $package:output:built $package:output ())
(define+provide-message $package:output:reused $package:output ())
(define+provide-message $package:output:undefined $package:output ())
(define+provide-message $package:definition $package ())
(define+provide-message $package:definition:value $package:definition (id))
(define+provide-message $package:definition:value:missing $package:definition:value ())
(define+provide-message $package:definition:value:invalid $package:definition:value (value))
(define+provide-message $package:definition:undeclared-racket-version $package:definition ())
(define+provide-message $package:definition:unsupported-racket-version $package:definition (versions))
(define+provide-message $package:definition:unsupported-os $package:definition (supported))

(define DEFAULT_OUTPUT "default")


;----------------------------------------------------------------------------
; High-level interface

(define (run-package pkg-definition-variant
                     #:output-name [output-name DEFAULT_OUTPUT]
                     #:link-path [link-path #f])
  (do pkgeval <- (make-package-evaluator pkg-definition-variant)
      (install-package-output (or link-path (pkgeval 'package))
                              output-name
                              pkgeval)))




;----------------------------------------------------------------------------
; The validation procedures make sure a user can get the output they
; want from a definition.

(define (validate-requested-output pkgeval output-name)
  (if (member output-name (cons DEFAULT_OUTPUT (xiden-evaluator-ref pkgeval 'outputs null)))
      (logged-unit output-name)
      (logged-failure ($package:output:undefined))))


(define (validate-evaluator-binding #:optional optional pkgeval id-sym valid?)
  (logged
   (λ (errors)
     (call/cc
      (λ (return)
        (let* ([fail (λ (m) (return FAILURE (cons m errors)))]
               [okay (λ () (return pkgeval errors))]
               [get (pkgeval '#%info-lookup)]
               [on-missing (λ () (if optional (okay) (fail ($package:definition:value:missing id-sym))))]
               [value (get id-sym on-missing)])
          (if (valid? value)
              (okay)
              (fail ($package:definition:value:invalid id-sym value)))))))))


(define (validate-evaluator pkgeval)
  (let ([assert (λ (#:optional [optional? #f] id-sym valid?)
                  (validate-evaluator-binding #:optional optional? pkgeval id-sym valid?))])
    (do (assert 'provider non-empty-string?)
        (assert 'package non-empty-string?)
        (assert 'edition non-empty-string?)
        (assert 'revision-number revision-number?)
        (assert 'inputs (listof concrete-input-info/c))
        (assert 'build (λ (p) (and (procedure? p) (= 1 (procedure-arity p)))))
        (assert #:optional #t 'outputs (listof non-empty-string?))
        (assert #:optional #t 'revision-names (listof non-empty-string?))
        (validate-os-support pkgeval)
        (if (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS)
            (assert #:optional #t 'racket-versions racket-version-ranges/c)
            (do (assert 'racket-versions racket-version-ranges/c)
                (validate-racket-support pkgeval))))))


(define (validate-os-support pkgeval)
  (logged
   (λ (messages)
     (let ([supported (xiden-evaluator-ref pkgeval 'os-support '(unix windows macosx))])
       (if (member (system-type 'os) supported)
           (values pkgeval messages)
           (values FAILURE
                   (cons ($package:definition:unsupported-os supported)
                         messages)))))))


(define (validate-racket-support pkgeval)
  (logged
   (λ (errors)
     (call/cc
      (λ (return)
        (define (okay) (values pkgeval errors))
        (define (fail m) (values FAILURE (cons m errors)))
        (let ([racket-support
               (check-racket-version-ranges
                (version)
                ((pkgeval '#%info-lookup) 'racket-versions (λ () null)))])
          (case racket-support
            [(supported) (okay)]
            [(unsupported)
             (if (XIDEN_ALLOW_UNSUPPORTED_RACKET)
                 (okay)
                 (fail ($package:definition:unsupported-racket-version racket-support)))]
            [(undeclared)
             (if (or (XIDEN_ALLOW_UNSUPPORTED_RACKET)
                     (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))
                 (okay)
                 (fail ($package:definition:undeclared-racket-version)))]
            [else (error)])))))))



;----------------------------------------------------------------------------
; Output processing

(define (install-package-output link-path output-name pkgeval)
  (let ([package-name (abbreviate-exact-xiden-query (package-evaluator->xiden-query pkgeval))])
    (logged-combine (do (validate-requested-output pkgeval output-name)
                        (install-output! pkgeval output-name link-path)
                        (return (logged-unit (kill-evaluator pkgeval))))
                    (λ (to-wrap messages)
                      (cons ($package:log package-name output-name to-wrap)
                            messages)))))


(define (install-output! pkgeval output-name link-path)
  (call-with-reused-output
   (package-evaluator->xiden-query pkgeval)
   output-name
   (λ (variant)
     (cond [(output-record? variant)
            (reuse-package-output! pkgeval output-name variant link-path)]
           [(exn? variant)
            (raise variant)]
           [else
            (build-package-output! pkgeval output-name link-path)]))))


(define (reuse-package-output! pkgeval output-name output-record-inst link-path)
  (logged
   (λ (messages)
     (define directory-record (find-path-record (output-record-path-id output-record-inst)))
     (make-addressable-link directory-record link-path)
     (values SUCCESS
             (cons ($package:output:reused)
                   messages)))))


(define (open-input-info-as-bytes info)
  (open-input-bytes
    (with-handlers ([values (λ (e) (string->bytes/utf-8 (input-info-name info)))])
      (integrity-info-digest (input-info-integrity info)))))


(define (build-package-output! pkgeval output-name link-path)
  (logged
   (λ (messages)
     (define directory-record
       (make-addressable-directory
        (cons (open-input-string output-name)
              (map open-input-info-as-bytes
                   (xiden-evaluator-ref pkgeval 'inputs null)))
        (λ (build-dir)
          (pkgeval `(current-directory ,build-dir))
          (pkgeval `(build ,output-name)))))

     (declare-output (xiden-evaluator-ref pkgeval 'provider)
                     (xiden-evaluator-ref pkgeval 'package)
                     (xiden-evaluator-ref pkgeval 'edition "default")
                     (xiden-evaluator-ref pkgeval 'revision-number)
                     (xiden-evaluator-ref pkgeval 'revision-names null)
                     output-name
                     directory-record)

     (make-addressable-link directory-record link-path)

     (values SUCCESS
             (cons ($package:output:built)
                   messages)))))


(define (package-evaluator->xiden-query pkgeval)
  (xiden-query (xiden-evaluator-ref pkgeval 'provider "")
               (xiden-evaluator-ref pkgeval 'package "")
               (xiden-evaluator-ref pkgeval 'edition "")
               (~a (xiden-evaluator-ref pkgeval 'revision-number ""))
               (~a (xiden-evaluator-ref pkgeval 'revision-number ""))
               "ii"))


;----------------------------------------------------------------------------
; Operations on package definitions, as evaluators

(define (make-package-evaluator source)
  (do sourced-eval <- (if (string? source)
                          (fetch-package-definition source)
                          (logged-unit (load-xiden-module (override-package-definition source))))
      (validate-evaluator sourced-eval)
      (return sourced-eval)))


(define (override-package-definition variant)
  (define plugin-override
    (load-from-plugin 'before-new-package
                      (λ () values)
                      (λ (e) values)))
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


(define (fetch-package-definition source)
  (logged
   (λ (m)
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

     (define-values (fetch-st messages) (run-log logged/fetch-st m))

     (values (if (fetch-state? fetch-st)
                 (or (fetch-state-result fetch-st)
		     FAILURE)
                 FAILURE)
             (cons messages m)))))


(module+ test
  (require racket/runtime-path
           rackunit
           (submod "file.rkt" test)
           "setting.rkt")

  (test-case "Check Racket version support"
    (define (make-dummy-pkginfo versions)
      (hash+list->xiden-evaluator
       (hash 'racket-versions versions
             'package-name "whatever")))

    (define (test-case/racket-version msg versions instance?)
      (test-case "Detect packages that do not declare a supported Racket version"
        (define pkginfo (make-dummy-pkginfo versions))
        (define-values (_ messages) (run-log (validate-racket-support pkginfo)))
        (check-pred instance? (car messages))))

    (test-case/racket-version
     "Detect packages that do not declare a supported Racket version"
     null
     $package:definition:undeclared-racket-version?)

    (test-case/racket-version
     "Detect packages that declare an unsupported Racket version"
     '("0.0")
     $package:definition:unsupported-racket-version?)))
