#lang racket/base

; Define runtime configuration.

(provide (all-defined-out))

(require racket/function
         racket/match
         racket/pretty
         (only-in racket/tcp listen-port-number?)
         "config.rkt"
         "contract.rkt"
         "setting.rkt"
         "string.rkt"
         "url.rkt"
         "workspace.rkt")


(define (in-xiden-setting-value-sources id default-value)
  (in-list (list (λ () (maybe-get-setting-value-from-envvar (symbol->string id)))
                 (λ () ((load-xiden-rcfile) id any/c (void)))
                 (const default-value))))


(define (xiden-setting-default-value default-value)
  (λ (id)
    (for/or ([make-value (in-xiden-setting-value-sources id default-value)])
      (define maybe-not-void (make-value))
      (and (not (void? maybe-not-void))
           maybe-not-void))))


(define-syntax-rule (define-xiden-setting id cnt short-flag default-value help-strs)
  (define-setting id cnt short-flag (xiden-setting-default-value default-value) help-strs))


(define (maybe-get-setting-value-from-envvar envname)
  (define env (getenv envname))
  (cond [(not env) (void)]
        [(string=? env "") (void)]
        [else (read (open-input-string env))]))


(define (make-xiden-settings-namespace)
  (define ns (make-base-namespace))
  (for ([(k v) (in-hash XIDEN_SETTINGS)])
    (namespace-set-variable-value! k v #t ns #t))
  (namespace-set-variable-value! 'save! save-xiden-settings! #t ns #t)
  (namespace-set-variable-value! 'dump
                                 (λ () (pretty-write #:newline? #t (dump-xiden-settings)))
                                 #t ns #t)
  ns)


(define (get-xiden-settings-path)
  (build-workspace-path "etc/xiden.rkt"))


(define load-xiden-rcfile
  (let ([cache #f])
    (λ (#:reload? [reload? #f])
      (when (or (not cache) reload?)
        (let ([path (get-xiden-settings-path)])
          (set! cache
                (if (file-exists? path)
                    (load-config path)
                    (make-config-closure (hasheq) null)))))
      cache)))


(define (get-xiden-setting . args)
  (apply hash-ref XIDEN_SETTINGS args))


(define (get-xiden-setting-value . args)
  ((apply get-xiden-setting args)))


(define (dump-xiden-settings)
  (for/hash ([(k v) (in-hash XIDEN_SETTINGS)])
    (values k (v))))


(define (save-xiden-settings! closure)
  (save-config! closure (get-xiden-settings-path)))


(define (switch-help strs)
  (cons strs (list "#t-or-#f")))


(define-xiden-setting XIDEN_SANDBOX_MEMORY_LIMIT_MB (>=/c 0) "-M" 30
  '("Total memory quota for a sandbox"
    "mibibytes"))


(define-xiden-setting XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB (>=/c 0) "-e" 10
  '("Memory quota for each sandboxed expression"
    "mibibytes"))


(define-xiden-setting XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS (>=/c 0) "-S" (* 5 60)
  '("Time limit for each sandboxed expression"
    "seconds"))


  ; Controls network and file I/O permissions for sandboxed installers.
(define-xiden-setting XIDEN_SANDBOX_NETWORK_PERMISSIONS
   (list/c (or/c #f string?)
           (or/c #f string?)
           (or/c #f string?)
           (or/c #f string?))
  "-N" '(#f #f #f #f)
  '("Regex patterns permissions"
    "string-list"))


(define-xiden-setting XIDEN_MATCH_RACKET_MODULES boolean? "-r" #f
  (switch-help "Match against .rkt, .ss, .scrbl, and .rktd."))


(define-xiden-setting XIDEN_MATCH_COMPILED_RACKET boolean? "-b" #f
  (switch-help "Match against .zo and .dep."))


(define-xiden-setting XIDEN_MODS_MODULE (or/c #f path-string?) "-M" #f
  '("A path to a module that extends Xiden."
    "path-or-#f"))


(define-xiden-setting XIDEN_SANDBOX_PATH_PERMISSIONS
  (listof (list/c (or/c 'execute 'write 'delete
                        'read-bytecode 'read 'exists)
                  (or/c byte-regexp? bytes? string? path?)))
  "-P" null
  '("A value for sandbox-path-permissions"
    "racket-value"))


; Scenario: Artifact does not have a signature. This is normal
; when prototyping or working with a trusted peer, so
; we'll prompt by default.
(define-xiden-setting XIDEN_TRUST_UNSIGNED boolean? "-U" #f
  (switch-help "Trust unsigned packages"))


; Scenario: Artifact signature cannot be verified with publisher's public key.
; This is more suspicious.
(define-xiden-setting XIDEN_TRUST_BAD_SIGNATURE boolean? #f #f
  (switch-help "Trust signatures that don't match provider's public key"))


(define-xiden-setting XIDEN_TRUST_UNVERIFIED_HOST boolean? #f #f
  (switch-help "Trust servers that do not have a valid certificate."))


; Halt when downloaded artifact does not pass integrity check
(define-xiden-setting XIDEN_TRUST_BAD_DIGEST boolean? "-D" #f
  (switch-help (format "Trust artifacts that don't pass an integrity check. Implies ~a."
                       (setting-short-flag XIDEN_TRUST_UNSIGNED))))


(define-xiden-setting XIDEN_FASL_OUTPUT boolean? "-F" #f
  (switch-help "Use FASL program output"))


(define-xiden-setting XIDEN_READER_FRIENDLY_OUTPUT boolean? "-R" #f
  (switch-help "Use (read)able program output"))


(define-xiden-setting XIDEN_VERBOSE boolean? "-v" #f
  (switch-help "Show more information in program output"))


(define-xiden-setting XIDEN_PORT listen-port-number? "-p" 8080
  '("Set listen port"
    "port-number"))


(define-xiden-setting XIDEN_PRIVATE_KEY_PATH (or/c #f path-string?) "-q" #f
  '("The location of a private key"
    "path"))


; Where to install packages.
(define-xiden-setting XIDEN_INSTALL_RELATIVE_PATH
  (and/c path-string? (not/c complete-path?))
  "-I"
  "usr/lib/racket"
  '("Workspace-relative path for installed packages"
    "relative-path-string"))


; Where to place launchers
(define-xiden-setting XIDEN_LAUNCHER_RELATIVE_PATH
  (and/c path-string? (not/c complete-path?))
  "bin"
  "-L"
  '("Workspace-relative path for launchers"
    "relative-path-string"))


(define-xiden-setting XIDEN_SERVICE_ENDPOINTS
  (listof url-string?)
  "-E" (list "https://zcpkg.com")
  '("Services to contact when searching for package definitions"
    "list"))


(define-xiden-setting XIDEN_LINK boolean? "-l" #f
  (switch-help "When installing a package on the filesystem, create a symlink to the source directory."))


(define-xiden-setting XIDEN_CONSENT boolean? "-y" #f
  (switch-help "Consent to overall task, but not to risky specifics."))


(define-xiden-setting XIDEN_DOWNLOAD_MAX_REDIRECTS exact-nonnegative-integer? "-r" 2
  '("Maximum redirects to follow when downloading an artifact"
    "exact-nonnegative-integer"))


(define-xiden-setting XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS boolean? "-u" #f
  (switch-help "Install packages even if they do not declare supported Racket versions"))


(define-xiden-setting XIDEN_ALLOW_UNSUPPORTED_RACKET boolean? #f #f
  (switch-help "Install packages even if they declare that they do not support the running version of Racket."))


(define-setting-group XIDEN_SETTINGS
  [XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS
   XIDEN_ALLOW_UNSUPPORTED_RACKET
   XIDEN_CONSENT
   XIDEN_DOWNLOAD_MAX_REDIRECTS
   XIDEN_FASL_OUTPUT
   XIDEN_INSTALL_RELATIVE_PATH
   XIDEN_LAUNCHER_RELATIVE_PATH
   XIDEN_LINK
   XIDEN_MATCH_COMPILED_RACKET
   XIDEN_MATCH_RACKET_MODULES
   XIDEN_MODS_MODULE
   XIDEN_PORT
   XIDEN_PRIVATE_KEY_PATH
   XIDEN_READER_FRIENDLY_OUTPUT
   XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB
   XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS
   XIDEN_SANDBOX_MEMORY_LIMIT_MB
   XIDEN_SANDBOX_NETWORK_PERMISSIONS
   XIDEN_SANDBOX_PATH_PERMISSIONS
   XIDEN_SERVICE_ENDPOINTS
   XIDEN_TRUST_BAD_DIGEST
   XIDEN_TRUST_BAD_SIGNATURE
   XIDEN_TRUST_UNSIGNED
   XIDEN_TRUST_UNVERIFIED_HOST
   XIDEN_VERBOSE])
