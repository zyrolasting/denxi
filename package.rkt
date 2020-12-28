#lang racket/base

; Define packages as active instantiations of package definitions.
; Interact with packages using sandboxed evaluators.

(provide install)

(require racket/function
         racket/format
         racket/sandbox
         version/utils
         "codec.rkt"
         "contract.rkt"
         "file.rkt"
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
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "url.rkt"
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
(define+provide-message $package:security $package (reporting-guard summary args))

(define DEFAULT_OUTPUT "default")


;----------------------------------------------------------------------------
; High-level interface

(define (install link-path-or-#f output-name-or-#f package-definition-source)
  (do package-definition-variant <- (derive-package-definition
                                     #:max-size (mebibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB))
                                     package-definition-source)

      pkgeval <- (make-package #:memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)
                               #:eval-memory-limit (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB)
                               #:eval-time-limit (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                               #:trusted-executables (XIDEN_TRUSTED_EXECUTABLES)
                               #:allowed-envvars (XIDEN_ALLOW_ENV)
                               #:trust-any-executable? (XIDEN_TRUST_ANY_EXECUTABLE)
                               #:workspace (workspace-directory)
                               #:attach-stdin? #f
                               package-definition-variant)

      ; Bind identifying information
      #:let [package-name (abbreviate-exact-xiden-query (package-evaluator->xiden-query pkgeval))]
      #:let [output-name  (or output-name-or-#f DEFAULT_OUTPUT)]
      #:let [link-path    (or link-path-or-#f (pkgeval 'package))]

      ; Scope all activities in a $package:log
      (logged-combine (do (validate-inputs pkgeval)
                          (validate-os-support pkgeval)
                          (validate-racket-support #:allow-unsupported? (XIDEN_ALLOW_UNSUPPORTED_RACKET)
                                                   pkgeval)
                          (fulfil-package-output pkgeval
                                                 output-name
                                                 link-path)
                          (return (logged-unit (kill-evaluator pkgeval))))
                      (λ (to-wrap messages)
                        (cons ($package:log package-name output-name to-wrap)
                              messages)))))




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


(define-logged (validate-racket-support #:allow-unsupported? allow-unsupported? pkgeval)
  (let ([racket-support (check-racket-version-ranges (version) (pkgeval 'racket-versions))])
    (case racket-support
      [(supported) ($use pkgeval)]
      [(unsupported)
       (if allow-unsupported?
           ($use pkgeval)
           ($fail ($package:unsupported-racket-version racket-support)))])))


;----------------------------------------------------------------------------
; Output installation
;
; When installing a package output, we can either reuse existing
; output files or build a new distribution of output files.

(define (fulfil-package-output pkgeval output-name link-path)
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
         (map open-input-info-as-bytes (pkgeval 'inputs)))
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
; Inflection point between privileged runtime and less-privileged runtime.

(define (make-package #:memory-limit memory-limit
                      #:eval-memory-limit eval-memory-limit
                      #:eval-time-limit eval-time-limit
                      #:trusted-executables trusted-executables
                      #:allowed-envvars allowed-envvars
                      #:trust-any-executable? trust-any-executable?
                      #:workspace workspace
                      #:attach-stdin? attach-stdin?
                      input-program)
  (parameterize ([sandbox-input (and attach-stdin? (current-input-port))]
                 [sandbox-output (current-output-port)]
                 [sandbox-error-output (current-error-port)]
                 [sandbox-path-permissions (derive-path-permissions)]
                 [sandbox-memory-limit memory-limit]
                 [sandbox-eval-limits (list eval-time-limit eval-memory-limit)]
                 [sandbox-namespace-specs (append (sandbox-namespace-specs) '(xiden/rc xiden/package))]
                 [sandbox-make-environment-variables (bind-envvar-subset (cons "PATH" allowed-envvars))]
                 [sandbox-security-guard (make-package-security-guard
                                          #:trust-any-executable? trust-any-executable?
                                          #:trust-executables trusted-executables
                                          #:workspace workspace)])
    (make-module-evaluator #:language PACKAGE_DEFINITION_MODULE_LANG
                           input-program)))


;-------------------------------------------------------------------------------
; Package definition discovery and overriding

(define (derive-package-definition #:max-size max-size source)
  (define plugin-override
    (load-from-plugin 'before-new-package
                      (λ () values)
                      (λ (e) values)))

  (define package-definition-read-source
    (if (string? source)
        (fetch-package-definition #:max-size max-size source)
        source))

  (dynamic-wind void
                (λ () (plugin-override (read-package-definition package-definition-read-source)))
                (λ ()
                  (when (input-port? package-definition-read-source)
                    (close-input-port package-definition-read-source)))))


(define-logged (fetch-package-definition #:max-size max-size source)
  (define logged/fetch-st
    (fetch source
           (list source)
           (λ (from-source est-size)
             (make-limited-input-port from-source
                                      (min max-size est-size)
                                      #t))))
  (define-values (fetch-st messages) ($run! logged/fetch-st))
  (if (and (fetch-state? fetch-st) (fetch-state-result fetch-st))
      ($use (fetch-state-result fetch-st) messages)
      ($use FAILURE messages)))



;-------------------------------------------------------------------------------
; Extract a selection of envvars for exposure to sandboxed evaluator.

(define (bind-envvar-subset allowed)
  (λ ()
    (apply make-environment-variables
           (for/fold ([mappings null])
                     ([unnormalized-name (in-list allowed)])
             (define name
               (if (string? unnormalized-name)
                   (string->bytes/utf-8 unnormalized-name)
                   unnormalized-name))
             (cons name
                   (cons (environment-variables-ref (current-environment-variables) name)
                         mappings))))))



;-------------------------------------------------------------------------------
; Security guard definition
;
; Packages in the Racket runtime should only concern themselves with
; writing files in designated workspace directories, running
; subprocesses that the user trusts, and downloading data under safety
; limits.
;
; The only way to block an unwanted operation is to raise a value.
; Note that I raise a message type to avoid breaking localization.
; Raising exceptions means imposing a language on the reader.

(define (make-package-security-guard #:trust-any-executable? trust-any-executable?
                                     #:trust-executables trust-executables
                                     #:workspace [ws (workspace-directory)])
  (make-security-guard
   (current-security-guard)
   (make-file-guard trust-executables (get-writable-workspace-directories ws))
   (make-network-guard)
   (make-link-guard ws)))


(define (make-file-guard #:trust-any-executable? trust-any-executable?
                         #:trust-executables trust-executables
                         #:writeable-directories write-dirs)
  (let ([trust-executable? (bind-trust-list trust-executables)])
    (λ (sym path-or-#f ops)
      (define (check-destructive-op op path)
        (define test (curry path-prefix? (normalize-path path)))
        (unless (ormap test write-dirs)
          (raise ($package:security 'file op (list sym path-or-#f ops)))))

      (when path-or-#f
        (cond [(member 'execute ops)
               (unless (or (equal? "openssl" (path->string (file-name-from-path path-or-#f)))
                           trust-any-executable?
                           (trust-executable? path-or-#f))
                 (raise ($package:security 'file
                                           'blocked-execute
                                           (list sym path-or-#f ops))))]

              [(member 'write ops)
               (check-destructive-op 'blocked-write path-or-#f)]

              [(member 'delete ops)
               (check-destructive-op 'blocked-delete path-or-#f)])))))


(define (make-network-guard)
  (λ (sym hostname-or-#f port-or-#f client-or-server)
    (unless hostname-or-#f
      (raise ($package:security 'network 'blocked-listen
                                (list sym hostname-or-#f port-or-#f client-or-server))))))


(define (make-link-guard workspace)
  (define (path-ok? p)
    (path-prefix? (simplify-path (if (complete-path? p) p (build-path workspace p)))
                  workspace))

  (λ (op link-path target-path)
    (unless (path-ok? (normalize-path target-path))
      (raise ($package:security 'link
                                'blocked-link
                                (list op link-path target-path))))))


(define (derive-path-permissions)
  (append (map (λ (p) `(exists ,p)) (filesystem-root-list/cached))
          (sandbox-path-permissions)))


(define (get-writable-workspace-directories [wd (workspace-directory)])
  (list (build-path wd "var/xiden")
        (build-path wd "tmp")))


(module+ test
  (require racket/runtime-path
           rackunit
           (submod "file.rkt" test)
           (submod "logged.rkt" test)
           "setting.rkt")

  (test-case "Check Racket version support"
    (define (make-dummy-pkginfo versions)

      (make-package-definition-datum
       `((racket-versions . ,versions))))

    (test-logged-procedure
     "Detect packages that declare an unsupported Racket version"
     (validate-racket-support #:allow-unsupported? #f (make-dummy-pkginfo '("0.0")))
     (λ (val msg)
       (check-pred $package:unsupported-racket-version?
                   (car msg))))))
