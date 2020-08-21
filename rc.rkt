#lang racket/base

; Define runtime configuration.

(require "contract.rkt")
(provide
 (contract-out
  [XIDEN_SETTINGS
   (hash/c symbol? setting? #:immutable #t)]
  [try-user-setting
   (-> setting? string? $with-output?)]
  [get-xiden-settings-path
   (-> complete-path?)]
  [load-xiden-rcfile
   (-> config-closure/c)]
  [dump-xiden-settings
   (-> (hash/c symbol? setting?))]
  [save-xiden-settings!
   (-> any)]))


(require racket/function
         racket/match
         racket/pretty
         (only-in racket/tcp listen-port-number?)
         "config.rkt"
         "output.rkt"
         "setting.rkt"
         "url.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")


; The only difference between a vanilla setting an a Xiden setting is how a
; fallback value is computed.
(define-syntax-rule (define-xiden-setting id cnt short-flag default-value help-strs)
  (define-setting id cnt short-flag (xiden-setting-find-value default-value) help-strs))


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
                 (λ () ((load-xiden-rcfile) id any/c (void)))
                 (const default-value))))


(define (maybe-get-setting-value-from-envvar envname)
  (define env (getenv envname))
  (cond [(not env) (void)]
        [(string=? env "") (void)]
        [else (read (open-input-string env))]))


; Map user setting exception to program output
(define (make-invalid-user-setting-handler target-setting string-value)
  (λ (e)
    (output-failure
     ($reject-user-setting
      (setting-id target-setting)
      string-value
      (if (exn:fail:contract? e)
          (cadr (regexp-match #px"expected:\\s+([^\n]+)"
                              (exn-message e)))
          (exn-message e))))))


; Use this to update a setting from string input (such as a command-line argument)
; Returns program output explaining why the value was rejected.
(define (try-user-setting target-setting string-value)
  (with-handlers ([exn:fail? (make-invalid-user-setting-handler target-setting string-value)])
    (define value (read (open-input-string string-value)))
    (target-setting
     value
     (λ () (output-return #:stop-value #f #f
                          ($accept-user-setting (setting-id target-setting) value))))))


(define (get-xiden-settings-path)
  (build-workspace-path "etc/xiden.rkt"))


; load-xiden-rcfile reads the rcfile every time, which avoids caching bugs.
;
; But combined with in-xiden-setting-value-sources, there are two
; other problems:
;
; 1. You trigger a full file read for every fetch of an undefined setting's value.
; 2. Different calls may return different values for the same setting
;
; #2 is desirable for secured interactive applications, but #1 is just wasteful.
; To avoid extra reads, create a parameterization where each setting
; has a value. e.g. (call-with-applied-settings all-settings (λ () ...))
;
; In that parameterization, no disk reads will occur because there
; will be no need to search for a value. Note that using
; (dump-xiden-settings) for `all-settings` above defeats the purpose
; because it will read the file once for every unset value anyway.
; But if you would trigger more reads than the dump would, perhaps
; that's not an issue.

(define (load-xiden-rcfile)
  (let ([path (get-xiden-settings-path)])
    (if (file-exists? path)
        (load-config path)
        (make-config-closure (hasheq) null))))


(define (dump-xiden-settings)
  (for/hash ([(k v) (in-hash XIDEN_SETTINGS)])
    (values k (v))))


; This implementation preserves the reading order of
; any rcfile on disk, as a courtesy to the user.
(define (save-xiden-settings!)
  (save-config! (make-config-closure
                 (dump-xiden-settings)
                 (filter (curry hash-has-key? XIDEN_SETTINGS)
                         ((load-xiden-rcfile) READ_ORDER)))
                (get-xiden-settings-path)))


; For boolean options, since they all use the same help string.
(define (switch-help str)
  (cons str '("#t-or-#f")))


;; Begin runtime configuration space
;; =================================

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

  (test-workspace "Represent a lack of rcfile as an empty hash"
    (check-equal? ((load-xiden-rcfile)) (hasheq)))


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

  (test-equal? "Accept valid user-provided settings"
               (try-user-setting XIDEN_PRIVATE_KEY_PATH "\"a\"")
               ($with-output #f #f (list ($accept-user-setting 'XIDEN_PRIVATE_KEY_PATH "a"))))

  (test-equal? "Reject invalid user-provided settings"
               (try-user-setting XIDEN_PRIVATE_KEY_PATH "123")
               ($with-output 1 #f (list ($reject-user-setting
                                         'XIDEN_PRIVATE_KEY_PATH
                                         "123"
                                         "(or/c void? (or/c #f path-string?))"))))

  (let ([malformed "\"({"])
    (test-equal? "Reject unreadable user-provided settings"
                 (try-user-setting XIDEN_PRIVATE_KEY_PATH malformed)
                 ($with-output 1 #f (list ($reject-user-setting
                                           'XIDEN_PRIVATE_KEY_PATH
                                           malformed
                                           (with-handlers ([exn:fail:read? exn-message])
                                             (read (open-input-string malformed))))))))

  (test-workspace "Find fallback values from several sources"
    (test-false "First use a hard-coded value"
                (XIDEN_PRIVATE_KEY_PATH))

    (test-case "Override hard-coded value with rcfile"
      (XIDEN_PRIVATE_KEY_PATH "foo"
                              (λ ()
                                (check-false ((load-xiden-rcfile) 'XIDEN_PRIVATE_KEY_PATH any/c #f))
                                (save-xiden-settings!)
                                (check-equal? ((load-xiden-rcfile) 'XIDEN_PRIVATE_KEY_PATH) "foo")))
      (check-equal? (XIDEN_PRIVATE_KEY_PATH) "foo"))

    (test-case "Override rcfile value with envvar value"
      (dynamic-wind (λ () (putenv "XIDEN_PRIVATE_KEY_PATH" "\"bar\""))
                    (λ () (check-equal? (XIDEN_PRIVATE_KEY_PATH) "bar"))
                    (λ ()
                      (putenv "XIDEN_PRIVATE_KEY_PATH" "")
                      (test-equal? "Do not use empty envvar strings as a source for settings"
                                   (XIDEN_PRIVATE_KEY_PATH)
                                   "foo")))))

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
