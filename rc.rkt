#lang racket/base

; Define runtime configuration.

(require "contract.rkt")
(provide
 with-xiden-rcfile
 (contract-out
  [setting-ref
   (-> (or/c string? symbol?)
       (or/c setting? #f))]
  [XIDEN_SETTINGS
   (hash/c symbol? setting? #:immutable #t)]
  [get-xiden-settings-path
   (-> complete-path?)]
  [load-xiden-rcfile
   (-> (-> any/c any))]
  [dump-xiden-settings
   (-> (hash/c symbol? any/c))]
  [save-xiden-settings!
   (-> any)]))


(require racket/function
         racket/match
         racket/pretty
         (only-in racket/tcp listen-port-number?)
         "exn.rkt"
         "message.rkt"
         "sandbox.rkt"
         "setting.rkt"
         "url.rkt"
         "workspace.rkt")

(define+provide-message $setting (name))
(define+provide-message $setting-accepted $setting (value))
(define+provide-message $no-setting-sources ())
(define+provide-message $setting-not-found $setting ())
(define+provide-message $setting-value-unreadable $setting (source-name))
(define+provide-message $setting-value-rejected $setting (value expected))


(define current-xiden-rcfile-cache (make-parameter void))


; The only difference between a vanilla setting an a Xiden setting is how a
; fallback value is computed.
(define-syntax-rule (define-xiden-setting id cnt kind flags default-value help-strs)
  (begin (provide (contract-out [id setting?]))
         (define-setting id cnt kind flags (xiden-setting-find-value default-value) help-strs)))


; Return a procedure to fetch a value for a setting if it is not already set.
(define (xiden-setting-find-value default-value)
  (λ (id)
    (for/or ([make-value (in-xiden-setting-value-sources id default-value)])
      (define maybe-not-void (make-value))
      (and (not (void? maybe-not-void))
           maybe-not-void))))


; Checks environment variable, then rcfile, then hard-coded value.
(define (in-xiden-setting-value-sources id default-value)
  (in-list (list (λ () (maybe-get-setting-value-from-envvar (symbol->string id)))
                 (λ () ((current-xiden-rcfile-cache) `(#%info-lookup ',id void)))
                 (const default-value))))


(define (maybe-get-setting-value-from-envvar envname)
  (define env (getenv envname))
  (cond [(not env) (void)]
        [(string=? env "") (void)]
        [else (read (open-input-string env))]))


(define (setting-ref name)
  (hash-ref XIDEN_SETTINGS
            (if (string? name)
                (string->symbol name)
                name)
            #f))


(define (get-xiden-settings-path)
  (build-workspace-path "etc/xiden.rkt"))


; Warning: This performs a full file read every time. Use a cache where relevant.
(define (load-xiden-rcfile)
  (let ([path (get-xiden-settings-path)])
    (if (file-exists? path)
        (load-xiden-module path)
        void)))


(define-syntax-rule (with-xiden-rcfile body ...)
  (parameterize ([current-xiden-rcfile-cache (load-xiden-rcfile)])
    body ...))


(define (dump-xiden-settings)
  (for/hash ([(k v) (in-hash XIDEN_SETTINGS)])
    (values k (v))))


; This implementation preserves the reading order of
; any rcfile on disk, as a courtesy to the user.
(define (save-xiden-settings!)
  (define domain-or-void ((load-xiden-rcfile) '(#%info-domain)))
  (save-xiden-module!
   (hash+list->xiden-evaluator (dump-xiden-settings) (if (void? domain-or-void) null domain-or-void))
   (get-xiden-settings-path)))


; For boolean options, since they all use the same help string.
(define (switch-help str)
  (cons str '("#t-or-#f")))


;; Begin runtime configuration space
;; =================================

(define-xiden-setting XIDEN_SANDBOX_MEMORY_LIMIT_MB (>=/c 0) 'once-each '("-M") 200
  '("Total memory quota for a sandbox"
    "mibibytes"))


(define-xiden-setting XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB (>=/c 0) 'once-each '("-e") 200
  '("Memory quota for each sandboxed expression"
    "mibibytes"))


(define-xiden-setting XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS (>=/c 0) 'once-each '("-S") (* 5 60)
  '("Time limit for each sandboxed expression"
    "seconds"))


; Controls network and file I/O permissions for sandboxed installers.
(define-xiden-setting XIDEN_SANDBOX_NETWORK_PERMISSIONS
   (list/c (or/c #f string?)
           (or/c #f string?)
           (or/c #f string?)
           (or/c #f string?))
  'multi
  '("-N")
  '(#f #f #f #f)
  '("Regex patterns permissions"
    "string-list"))


(define-xiden-setting XIDEN_INSTALL_SOURCES (listof string?) 'multi '("-i") null
  '("Add installation to transaction"
    "source"))


(define-xiden-setting XIDEN_UNINSTALL_SOURCES (listof string?) 'multi '("-u") null
  '("Add an uninstallation to transaction"
    "source"))


(define-xiden-setting XIDEN_MATCH_RACKET_MODULES boolean? 'once-each '("-r") #f
  (switch-help "Match .rkt, .ss, .scrbl, and .rktd"))


(define-xiden-setting XIDEN_MATCH_COMPILED_RACKET boolean? 'once-each '("-b") #f
  (switch-help "Match .zo and .dep."))


(define-xiden-setting XIDEN_MODS_MODULE (or/c #f path-string?) 'once-each '("-M") #f
  '("A path to a module that extends Xiden."
    "path-or-#f"))


(define-xiden-setting XIDEN_SANDBOX_PATH_PERMISSIONS
  (listof (list/c (or/c 'execute 'write 'delete
                        'read-bytecode 'read 'exists)
                  (or/c byte-regexp? bytes? string? path?)))
  'multi '("-P") null
  '("Add entry to sandbox-path-permissions"
    "racket-value"))


; Scenario: Artifact does not have a signature. This is normal
; when prototyping or working with a trusted peer, so
; we'll prompt by default.
(define-xiden-setting XIDEN_TRUST_UNSIGNED boolean? 'once-each '("-U") #f
  (switch-help "Trust unsigned packages"))


; Scenario: Artifact signature cannot be verified with publisher's public key.
; This is more suspicious.
(define-xiden-setting XIDEN_TRUST_BAD_SIGNATURE boolean? 'once-each '("--trust-bad-signature") #f
  (switch-help "Trust signatures that don't match public key"))


(define-xiden-setting XIDEN_TRUST_UNVERIFIED_HOST boolean? 'once-each '("--trust-any-host") #f
  (switch-help "Download from any server without authenticating"))


(define-xiden-setting XIDEN_TRUST_BAD_DIGEST boolean? 'once-each '("--trust-any-digest") #f
  (switch-help (format "(DANGEROUS) Trust any input. Implies ~a."
                       (setting-short-flag XIDEN_TRUST_UNSIGNED))))


(define-xiden-setting XIDEN_FASL_OUTPUT boolean? 'once-each '("-F") #f
  (switch-help "Use FASL program output"))


(define-xiden-setting XIDEN_FETCH_TOTAL_SIZE_MB (or/c +inf.0 real?) 'once-each null 100
  '("Maximum size, in mibibytes, to read from a source. +inf.0 = no limit"
    "mibibytes-or-+inf.0"))


(define-xiden-setting XIDEN_FETCH_BUFFER_SIZE_MB (real-in 0.1 20) 'once-each null 10
  '("Buffer size, in mibibytes, used when reading bytes"
    "mibibytes"))


(define-xiden-setting XIDEN_FETCH_PKGDEF_SIZE_MB (real-in 0.1 20) 'once-each null 0.1
  '("The maximum expected size, in mibibytes, of a package definition when scoping out work"
    "mibibytes"))


(define-xiden-setting XIDEN_FETCH_TIMEOUT_MS (real-in 100 (* 1000 10)) 'once-each null 3000
  '("The maximum time, in milliseconds, to wait for a distinct read of bytes from a source"
    "milliseconds"))


(define-xiden-setting XIDEN_READER_FRIENDLY_OUTPUT boolean? 'once-each '("-R") #f
  (switch-help "Use (read)able program output"))


(define-xiden-setting XIDEN_VERBOSE boolean? 'once-each '("-v") #f
  (switch-help "Show more information in program output"))


(define-xiden-setting XIDEN_PRIVATE_KEY_PATH (or/c #f path-string?) 'once-each '("-q") #f
  '("The location of a private key"
    "path"))


; Where to install packages.
(define-xiden-setting XIDEN_INSTALL_RELATIVE_PATH
  (and/c path-string? (not/c complete-path?))
  'once-each
  '("-I")
  "usr/lib/racket"
  '("Workspace-relative path for installed packages"
    "relative-path-string"))


; Where to place launchers
(define-xiden-setting XIDEN_LAUNCHER_RELATIVE_PATH
  (and/c path-string? (not/c complete-path?))
  'once-each
  '("-L")
  "bin"
  '("Workspace-relative path for launchers"
    "relative-path-string"))


(define-xiden-setting XIDEN_SERVICE_ENDPOINTS
  (listof url-string?)
  'once-each
  '("-E")
  '("https://zcpkg.com")
  '("Services to contact when searching for package definitions"
    "list"))


(define-xiden-setting XIDEN_LINK boolean? 'once-each '("-l") #f
  (switch-help "When installing a package on the filesystem, create a symlink to the source directory."))


(define-xiden-setting XIDEN_CONSENT boolean? 'once-each '("-y") #f
  (switch-help "Consent to overall task, but not to risky specifics."))


(define-xiden-setting XIDEN_DOWNLOAD_MAX_REDIRECTS exact-nonnegative-integer? 'once-each '("-r") 2
  '("Maximum redirects to follow when downloading an artifact"
    "exact-nonnegative-integer"))


(define-xiden-setting XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS boolean? 'once-each '("--allow-undeclared-racket") #f
  (switch-help "Install packages even if they do not declare supported Racket versions"))


(define-xiden-setting XIDEN_ALLOW_UNSUPPORTED_RACKET boolean? 'once-each '("--assume-support") #f
  (switch-help "Install packages even if they declare that they do not support the running version of Racket."))


(define-setting-group XIDEN_SETTINGS
  [XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS
   XIDEN_ALLOW_UNSUPPORTED_RACKET
   XIDEN_CONSENT
   XIDEN_DOWNLOAD_MAX_REDIRECTS
   XIDEN_FASL_OUTPUT
   XIDEN_FETCH_BUFFER_SIZE_MB
   XIDEN_FETCH_PKGDEF_SIZE_MB
   XIDEN_FETCH_TIMEOUT_MS
   XIDEN_FETCH_TOTAL_SIZE_MB
   XIDEN_INSTALL_RELATIVE_PATH
   XIDEN_LAUNCHER_RELATIVE_PATH
   XIDEN_LINK
   XIDEN_MATCH_COMPILED_RACKET
   XIDEN_MATCH_RACKET_MODULES
   XIDEN_MODS_MODULE
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

(module+ test
  (require rackunit
           (submod "file.rkt" test))


  (test-not-exn "Group all runtime configuration in a hash table"
                (λ ()
                  (invariant-assertion
                   (and/c immutable? (hash/c symbol? setting?))
                   XIDEN_SETTINGS)))


  (test-case "Define runtime configuration file path in terms of workspace"
    (define rcfile-/a
      (parameterize ([workspace-directory "/a"])
        (get-xiden-settings-path)))
    (define rcfile-/b
      (parameterize ([workspace-directory "/b"])
        (get-xiden-settings-path)))
    (check-pred complete-path? rcfile-/a)
    (check-pred complete-path? rcfile-/b)
    (check-not-equal? rcfile-/a rcfile-/b))


  (test-case "Dump current runtime configuration to a hash table"
    (check-not-exn
     (λ ()
       (invariant-assertion
        (and/c immutable? (hash/c symbol? (not/c setting?)))
        (dump-xiden-settings))))

    (define dump (dump-xiden-settings))
    (check-equal? (hash-ref dump 'XIDEN_PRIVATE_KEY_PATH)
                  (XIDEN_PRIVATE_KEY_PATH))

    (XIDEN_PRIVATE_KEY_PATH
     "foo"
     (λ ()
       (check-not-equal? (hash-ref dump 'XIDEN_PRIVATE_KEY_PATH)
                         (XIDEN_PRIVATE_KEY_PATH))
       (check-equal? (hash-ref (dump-xiden-settings) 'XIDEN_PRIVATE_KEY_PATH)
                     (XIDEN_PRIVATE_KEY_PATH)))))

  (test-workspace "Find fallback values from several sources"
    (test-false "First use a hard-coded value"
                (XIDEN_PRIVATE_KEY_PATH))

    (test-case "Override hard-coded value with rcfile"
      (XIDEN_PRIVATE_KEY_PATH "foo" save-xiden-settings!)
      (with-xiden-rcfile (check-equal? (XIDEN_PRIVATE_KEY_PATH) "foo")))

    (test-case "Override rcfile value with envvar value"
      (dynamic-wind (λ () (putenv "XIDEN_PRIVATE_KEY_PATH" "\"bar\""))
                    (λ () (check-equal? (XIDEN_PRIVATE_KEY_PATH) "bar"))
                    (λ ()
                      (putenv "XIDEN_PRIVATE_KEY_PATH" "")
                      (with-xiden-rcfile
                        (test-equal? "Do not use empty envvar strings as a source for settings"
                                     (XIDEN_PRIVATE_KEY_PATH)
                                     "foo"))))))

  (test-workspace "Lazily validate fallback values"
    (test-exn "Reject invalid values from ennvar"
              exn:fail:contract?
              (λ ()
                (dynamic-wind void
                              (λ ()
                                (putenv "XIDEN_PRIVATE_KEY_PATH" "123")
                                (XIDEN_PRIVATE_KEY_PATH))
                              (λ ()
                                (putenv "XIDEN_PRIVATE_KEY_PATH" "")))))))
