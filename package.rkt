#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/list
         racket/path
         racket/pretty
         racket/sequence
         net/head
         version/utils
         "contract.rkt"
         "encode.rkt"
         "exn.rkt"
         "file.rkt"
         "format.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "mod.rkt"
         "monad.rkt"
         "path.rkt"
         "port.rkt"
         "printer.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "sandbox.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "team.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt")

(define+provide-message $consent-note ())
(define+provide-message $no-package-info (source))
(define+provide-message $package (info))
(define+provide-message $package-installed $package ())
(define+provide-message $package-not-installed $package ())
(define+provide-message $undeclared-racket-version $package ())
(define+provide-message $unsupported-racket-version $package (versions))
(define+provide-message $undefined-package-output $package (name))
(define+provide-message $package-malformed $package (errors))


(define (install-package-from-source source expected-outputs)
  (do pkgeval         <- (make-package-evaluator source)
      checked-outputs <- (validate-output-request pkgeval expected-outputs)
      build-output    <- (build-package pkgeval checked-outputs)
      (return (report-installation-results pkgeval build-output))))


(define (make-package-evaluator source)
  (do sourced-eval    <- (fetch-package-definition source)
      validated-eval  <- (validate-evaluator sourced-eval)
      supported-eval  <- (check-racket-support validated-eval)
      (return supported-eval)))


(define (validate-evaluator pkgeval)
  (define errors null)
  (define (assert #:optional? [optional? #f] k predicate msg)
    (with-handlers ([values (λ (e) (unless optional? (set! errors (cons (exn-message e) errors))))])
      (define v (pkgeval k))
      (unless (predicate v)
        (set! errors (cons (format "~a: Expected ~a. Got ~e" k msg v)
                           errors)))))

  (assert 'provider string? "a string")
  (assert 'package string? "a string")
  (assert 'edition string? "a string")
  (assert 'revision-number exact-nonnegative-integer? "an exact, nonnegative integer")
  (assert 'inputs (listof well-formed-input-info/c) "a list of inputs")
  (assert 'outputs (listof name-string?) "a list of valid directory names")
  (assert 'build
          (λ (p) (and (procedure? p) (= 1 (procedure-arity p))))
          "a unary procedure")

  (assert #:optional? #t 'revision-names (listof string?) "a list of strings")
  (assert #:optional? (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS)
          'racket-versions
          racket-version-ranges/c
          "a list of Racket version range pairs, e.g. '((\"7.0\" . \"7.8\") ...) (Use #f to remove bound).")

  (if (null? errors)
      (logged-unit pkgeval)
      (logged-failure ($package-malformed (package-name pkgeval) errors))))


(define (package-name seval)
  (seval `(#%info-lookup 'package ,(λ () "???"))))


(define (report-installation-results pkgeval build-output)
  (logged-attachment build-output
                     (if (eq? build-output SUCCESS)
                         ($package-installed (package-name pkgeval))
                         ($package-not-installed (package-name pkgeval)))))


(define (validate-output-request pkgeval outs [original-outputs outs])
  (cond [(null? outs)
         (logged-unit original-outputs)]

        [(member (car outs)
                 (pkgeval 'outputs))
         (validate-output-request pkgeval
                                  (cdr outs)
                                  original-outputs)]

        [else
         (logged-failure
          ($undefined-package-output
           (package-name pkgeval)
           (car outs)))]))


(define (build-package pkgeval expected-outputs)
  (logged
   (λ (messages)
     ; Instrument top-level bindings as a hash table because evaluator calls cannot nest.
     (pkgeval `(current-info-lookup
                (let ([h ,(xiden-evaluator->hash pkgeval)])
                  (λ (k f) (hash-ref h k f)))))
     (dynamic-wind
       void
       (λ ()
         (values SUCCESS
                 (cons (for/list ([output (in-list expected-outputs)])
                         (build-package-output pkgeval output))
                       messages)))
       (λ () (kill-evaluator pkgeval))))))


; This is the heart of filesystem changes for a package installation.
; It concludes by declaring all related info in the database. If
; that succeeds and the program survives to the end of the database
; transaction, the whole operation is successful.
(define (build-package-output pkgeval output-name)
  (define directory-record
    (make-addressable-directory
     (λ (build-dir)
       (pkgeval `(cd ,build-dir))
       (pkgeval `(build ,output-name)))))

  (displayln directory-record)

  (declare-derivation (xiden-evaluator-ref pkgeval 'provider)
                      (xiden-evaluator-ref pkgeval 'package)
                      (xiden-evaluator-ref pkgeval 'edition "draft")
                      (xiden-evaluator-ref pkgeval 'revision-number)
                      (xiden-evaluator-ref pkgeval 'revision-names null)
                      directory-record))


; This is the inflection point between restricted and unrestricted
; resources for an evaluator.
(define (call-with-build-sandbox-parameterization proc)
  (parameterize ([sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-security-guard
                  (make-security-guard
                   (current-security-guard)
                   (make-pkgeval-file-guard (make-bin-path-permissions '("openssl"))
                                            (build-workspace-path "var/xiden"))
                   (make-pkgeval-network-guard)
                   (make-pkgeval-link-guard (workspace-directory)))]

                 [sandbox-make-environment-variables
                  (bind-envvar-subset '(#"PATH"))]
                 [sandbox-namespace-specs
                  (append (sandbox-namespace-specs)
                          '(xiden/rc xiden/package))])
    (proc)))



(define (make-pkgeval-file-guard allowed-executables write-dir)
  (λ (sym path-or-#f ops)
    (when path-or-#f
      (cond [(member 'execute ops)
             (unless (member path-or-#f allowed-executables)
               (raise-user-error 'security
                                 "Unauthorized attempt to execute ~a"
                                 path-or-#f))]

            [(member 'write ops)
             (unless (path-prefix? (normalize-path path-or-#f) write-dir)
               (raise-user-error 'security
                                 "Unauthorized attempt to write in ~a"
                                 path-or-#f))]

            [(member 'delete ops)
             (raise-user-error 'security
                               "Unauthorized attempt to delete ~a"
                               path-or-#f)]))))


(define (make-pkgeval-network-guard)
  (λ (sym hostname-or-#f port-or-#f client-or-server)
    (unless hostname-or-#f
      (raise-user-error 'security
                        "Unauthorized attempt to listen for connections"))
    ; TODO: Certificate checks, etc.
    ))

(define (make-pkgeval-link-guard workspace)
  (define (path-ok? p)
    (path-prefix? (simplify-path (if (complete-path? p) p (build-path workspace p)))
                  workspace))

  (λ (op link-path target-path)
    (unless (and (path-ok? target-path) (path-ok? link-path))
      (raise-user-error 'security
                        "Cannot create link. Both paths must be in ~a~n  link path: ~a~n  target path: ~a"
                        workspace
                        link-path
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
                    (make-limited-input-port from-source
                                             (min (mibibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB))
                                                  est-size)
                                             #t)))))))

     (define-values (fetch-st messages) (run-log logged/fetch-st m))

     (values (or (fetch-state-result fetch-st) FAILURE)
             (cons messages m)))))


(define (check-racket-support pkgeval)
  (let ([racket-support
         (check-racket-version-ranges
          (version)
          (pkgeval 'racket-versions))])
    (case racket-support
      [(supported)
       (logged-unit pkgeval)]
      [(unsupported)
       (if (XIDEN_ALLOW_UNSUPPORTED_RACKET)
           (logged-unit pkgeval)
           (logged-failure ($unsupported-racket-version
                            (package-name pkgeval)
                            (pkgeval 'racket-versions))))]
      [(undeclared)
       (if (or (XIDEN_ALLOW_UNSUPPORTED_RACKET)
               (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))
           (logged-unit pkgeval)
           (logged-failure ($undeclared-racket-version (package-name pkgeval))))])))


(define-message-formatter format-package-message
  [($package-installed name)
   (format "Installed package ~a"
           name)]

  [($undeclared-racket-version info)
   (join-lines
    (list (format "~a does not declare a supported Racket version."
                  info)
          (format "To install this package anyway, run again with ~a"
                  (setting-short-flag XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))))]

  [($package-malformed name errors)
   (format "~a has an invalid definition. Here are the errors for each field:~n~a"
           name
           (join-lines (indent-lines errors)))]

  [($unsupported-racket-version name versions)
   (join-lines
    (list (format "~a claims that it does not support this version of Racket (~a)."
                  name
                  (version))
          (format "Supported versions (ranges are inclusive):~n~a~n"
                  (join-lines
                   (map (λ (variant)
                          (format "  ~a"
                                  (if (pair? variant)
                                      (format "~a - ~a"
                                              (or (car variant)
                                                  PRESUMED_MINIMUM_RACKET_VERSION)
                                              (or (cdr variant)
                                                  PRESUMED_MAXIMUM_RACKET_VERSION))
                                      variant))
                          versions))))
          (format "To install this package anyway, run again with ~a"
                  (setting-long-flag XIDEN_ALLOW_UNSUPPORTED_RACKET))))])


(module+ test
  (require racket/runtime-path
           rackunit
           (submod "file.rkt" test)
           "setting.rkt")

  (test-case "Reject requests for outputs that a package does not define"
    (define (make-dummy-pkginfo outputs)
      (hash+list->xiden-evaluator
       (hash 'outputs outputs
             'package-name "whatever")))

    (define pkginfo (make-dummy-pkginfo '("lib" "doc" "test")))
    (check-equal? (get-log (validate-output-request pkginfo '("blah")))
                  (list ($undefined-package-output (package-name pkginfo) "blah")))

    (test-case "Allow requests for outputs that a package does define"
      (define request '("test" "lib" "doc"))
      (call-with-values (λ () (run-log (validate-output-request pkginfo request)))
                        (λ (v m)
                          (check-equal? v request)
                          (check-pred null? m)))))

  (test-case "Check Racket version support"
    (define (make-dummy-pkginfo versions)
      (hash+list->xiden-evaluator
       (hash 'racket-versions versions
             'package-name "whatever")))

    (test-case "Detect packages that do not declare a supported Racket version"
      (define pkginfo (make-dummy-pkginfo null))
      (check-equal? (get-log (check-racket-support pkginfo))
                    (list ($undeclared-racket-version (package-name pkginfo)))))

    (test-case "Detect packages that declare an unsupported Racket version"
      (define pkginfo (make-dummy-pkginfo (list "0.0")))
      (check-equal? (get-log (check-racket-support pkginfo))
                    (list ($unsupported-racket-version (package-name pkginfo)
                                                       (pkginfo 'racket-versions)))))))
