#lang racket/base

; Define packages as active instantiations of package definitions.
; Interact with packages using sandboxed evaluators.
;
; This module follows a different style due to its complexity.
;
; - The comments double as table of contents for following code.
; - Multiple test submodules are allowed.
;


(provide install)

(require racket/function
         racket/format
         racket/list
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


(module+ test
  (require rackunit
           (submod "file.rkt" test)
           (submod "logged.rkt" test)
           "setting.rkt"))


(define+provide-message $package ())
(define+provide-message $package:log $package (query output-name messages))
(define+provide-message $package:unfetched $package (source))
(define+provide-message $package:output $package ())
(define+provide-message $package:output:built $package:output ())
(define+provide-message $package:output:reused $package:output ())
(define+provide-message $package:output:undefined $package:output ())
(define+provide-message $package:abstract-input $package (versions))
(define+provide-message $package:unsupported-racket-version $package (versions))
(define+provide-message $package:unsupported-os $package (supported))
(define+provide-message $package:security $package (reporting-guard summary args))

(define DEFAULT_OUTPUT "default")


;===============================================================================
; Take care not to use XIDEN_* settings anywhere else in this
; module. The more they spread out, the harder it gets to predict how
; a process will behave.
;

(define (install link-path-or-#f output-name-or-#f package-definition-source)
  (mdo ; Sec. 1
       pkgdef := (get-package-definition package-definition-source
                                         (mebibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB)))

       ; Sec. 2
       pkgeval := (logged-unit
                   (make-package #:memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)
                                 #:eval-memory-limit (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB)
                                 #:eval-time-limit (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                 #:trusted-executables (XIDEN_TRUSTED_EXECUTABLES)
                                 #:allowed-envvars (XIDEN_ALLOW_ENV)
                                 #:trust-any-executable? (XIDEN_TRUST_ANY_EXECUTABLE)
                                 #:workspace (workspace-directory)
                                 #:attach-stdin? #f
                                 pkgdef))

       ; Sec. 3
       (fulfil-package-output #:allow-unsupported-racket? (XIDEN_ALLOW_UNSUPPORTED_RACKET)
                              (abbreviate-exact-xiden-query (package-evaluator->xiden-query pkgeval))
                              (or output-name-or-#f DEFAULT_OUTPUT)
                              (or link-path-or-#f (pkgeval 'package))
                              pkgeval)

       ; Sec. 4
       (clean-up pkgeval)

       (logged-unit SUCCESS)))




;===============================================================================
; 1: MAKE PACKAGE DEFINITION
;
; The user provides a package definition source. If it's a string,
; treat it like a source for a package input. Otherwise, try to use
; the source as an input program.
;
; The RC may override the definition to resolve abstract inputs,
; standardize dependencies, etc.
;

(define (get-package-definition source max-size)
  (mdo pkgdef := (find-original-package-definition source max-size)
       (logged-unit (override-package-definition pkgdef))))


(define (find-original-package-definition source max-size)
  (if (string? source)
      (mdo variant := (fetch-package-definition source max-size)
           (if (and (fetch-state? variant) (fetch-state-result variant))
               (read-package-definition (fetch-state-result variant))
               (logged-failure ($package:unfetched source))))
      (read-package-definition source)))


; TODO: Allow per-input overrides using RC
(define (override-package-definition datum)
  ((load-plugin-override) datum))


(define (fetch-package-definition source max-size)
  (fetch source
         (list source)
         (λ (from-source est-size)
           (make-limited-input-port from-source
                                    (min max-size est-size)
                                    #f))))

(define (load-plugin-override)
  (load-from-plugin 'before-new-package
                    (λ () values)
                    (λ (e) values)))



;===============================================================================
; 2: MAKE PACKAGE
;
; make-package acts as inflection point between a privileged runtime
; and a less-privileged runtime.  This is not a substitute for
; OS-level security. All of the code in this section deals with
; restricting a package evaluator.

(define (make-package #:memory-limit memory-limit
                      #:eval-memory-limit eval-memory-limit
                      #:eval-time-limit eval-time-limit
                      #:trusted-executables trusted-executables
                      #:allowed-envvars allowed-envvars
                      #:trust-any-executable? trust-any-executable?
                      #:workspace workspace
                      #:attach-stdin? attach-stdin?
                      input-program)
  ; 2.1
  (parameterize ([sandbox-input (and attach-stdin? (current-input-port))]
                 [sandbox-output (current-output-port)]
                 [sandbox-error-output (current-error-port)]
                 [sandbox-path-permissions (derive-path-permissions)]
                 [sandbox-memory-limit memory-limit]
                 [sandbox-eval-limits (list eval-time-limit eval-memory-limit)]
                 [sandbox-namespace-specs (append (sandbox-namespace-specs)
                                                  '(racket/base xiden/rc xiden/package))]
                 [sandbox-make-environment-variables (λ () (make-envvar-subset allowed-envvars))]
                 [sandbox-security-guard (make-package-security-guard ; 2.2
                                          #:trust-any-executable? trust-any-executable?
                                          #:trust-executables trusted-executables
                                          #:workspace workspace)])
    (make-module-evaluator #:language PACKAGE_DEFINITION_MODULE_LANG
                           input-program)))


(define (make-trusted-package input-program)
  (make-package #:memory-limit 2000
                #:eval-memory-limit 2000
                #:eval-time-limit 1000
                #:trusted-executables null
                #:allowed-envvars null
                #:trust-any-executable? #t
                #:workspace (workspace-directory)
                #:attach-stdin? #f
                input-program))


;-------------------------------------------------------------------------------
; 2.1: Sandbox permissions (sans-security guard)
;
; Extract a selection of envvars for exposure to sandboxed evaluator

(define (make-envvar-subset allowed [input-set (current-environment-variables)])
  (define subset-names (remove-duplicates (map coerce-bytes (cons "PATH" allowed))))
  (apply make-environment-variables
         (for/fold ([mappings null])
                   ([name (in-list subset-names)])
           (define value (environment-variables-ref input-set name))
           (if value
               (cons name
                     (cons (environment-variables-ref input-set name)
                           mappings))
               mappings))))


(define (derive-path-permissions)
  (append (map (λ (p) `(exists ,p)) (filesystem-root-list/cached))
          (sandbox-path-permissions)))


(define (get-writable-workspace-directories [wd (workspace-directory)])
  (list (build-path wd "var/xiden")
        (build-path wd "tmp")))


(module+ test
  (test-case "Make envvar subset"
    (define subset
      (make-envvar-subset
       '(#"bar")
       (make-environment-variables #"foo" #"1"
                                   #"bar" #"2"
                                   #"baz" #"3")))

    (check-false (environment-variables-ref subset #"foo"))
    (check-false (environment-variables-ref subset #"baz"))
    (check-equal? (environment-variables-ref subset #"bar") #"2"))

  (test-not-exn "Compute acceptable path permissions"
                (λ ()
                  (parameterize ([sandbox-path-permissions (derive-path-permissions)])
                    (void)))))


;-------------------------------------------------------------------------------
; 2.2: Security guard
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
   (make-file-guard #:trust-any-executable? trust-any-executable?
                    #:trust-executables trust-executables
                    #:writeable-directories (get-writable-workspace-directories ws))
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




;===============================================================================
; 3: OUTPUT FULFILMENT

(define (fulfil-package-output #:allow-unsupported-racket? allow-unsupported-racket?
                               package-name
                               output-name
                               link-path
                               pkgeval)
  (logged-combine (mdo ; 3.1
                   (validate-inputs pkgeval)
                   (validate-os-support pkgeval)
                   (validate-racket-support #:allow-unsupported? allow-unsupported-racket?
                                            pkgeval)
                   ; 3.2
                   (reuse-or-build-package-output pkgeval output-name link-path))
                  (λ (to-wrap messages)
                    (cons ($package:log package-name output-name (reverse (flatten to-wrap)))
                          messages))))


;-------------------------------------------------------------------------------
; 3.1: Validation
;
; Makes sure that the package agrees with the environment and runtime
; configuration.

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



(module+ test
  (test-case "Check Racket version support"
    (define (make-dummy-pkginfo versions)
      (make-package-definition-datum
       `((racket-versions . ,versions))))

    (define with-unsupported-version (make-trusted-package (make-dummy-pkginfo '("0.0"))))

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

(define (reuse-or-build-package-output pkgeval output-name link-path)
  (call-with-reused-output
   (package-evaluator->xiden-query pkgeval)
   output-name
   (λ (variant)
     (cond [(exn? variant)
            (raise variant)]
           [(output-record? variant)
            (reuse-package-output pkgeval output-name variant link-path)]
           [else
            (mdo directory-record := (build-package-output-directory pkgeval output-name)

                 (build-package-output (build-workspace-path (path-record-path directory-record))
                                       pkgeval
                                       output-name
                                       link-path)

                 (record-package-output pkgeval
                                        output-name
                                        directory-record
                                        link-path))]))))


(define-logged (reuse-package-output pkgeval output-name output-record-inst link-path)
  ($attach (make-addressable-link (find-path-record (output-record-path-id output-record-inst)) link-path)
           ($package:output:reused)))


(define-logged (build-package-output-directory pkgeval output-name)
  ($attach
   (make-addressable-directory
    (cons (open-input-string output-name)
          (map open-input-info-as-bytes (pkgeval 'inputs))))))


(define (open-input-info-as-bytes info)
  (open-input-bytes
    (with-handlers ([values (λ (e) (string->bytes/utf-8 (input-info-name info)))])
      (integrity-info-digest (input-info-integrity info)))))


(define-logged (build-package-output build-directory pkgeval output-name link-path)
  (define program (pkgeval `(build ,output-name)))
  (if program
      (parameterize ([current-directory build-directory])
        (pkgeval `(current-directory ,build-directory))
        ($run! program))
      ($fail ($package:output:undefined))))


(define-logged (record-package-output pkgeval output-name directory-record link-path)
  (declare-output (pkgeval 'provider)
                  (pkgeval 'package)
                  (pkgeval 'edition)
                  (pkgeval 'revision-number)
                  (pkgeval 'revision-names)
                  output-name
                  directory-record)
  (make-addressable-link directory-record link-path)
  ($use (void)))



;===============================================================================
; 4: CLEAN UP

(define (clean-up pkgeval)
  (kill-evaluator pkgeval)
  (collect-garbage)
  (logged-unit (void)))


;===============================================================================
; A: Supporting procedures

; Used to identify a package when reusing output, or when
; communicating status to the user.
(define (package-evaluator->xiden-query pkgeval)
  (xiden-query (pkgeval 'provider)
               (pkgeval 'package)
               (pkgeval 'edition)
               (~a (pkgeval 'revision-number))
               (~a (pkgeval 'revision-number))
               "ii"))

(module+ test
  (test-equal? "Compute package name"
               (package-evaluator->xiden-query
                (make-trusted-package
                   (make-package-definition-datum
                    `((provider "acme")
                      (package "anvil")
                      (edition "draft")
                      (revision-number 1)))))
               (xiden-query "acme" "anvil" "draft" "1" "1" "ii")))
