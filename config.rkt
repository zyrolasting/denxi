#lang racket/base

(provide confirmation-answer?
         user-consents?)

(require racket/contract
         racket/format
         racket/match
         "setting.rkt"
         "url.rkt"
         "workspace.rkt"
         "logging.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx))

(define (make-workspace-rcfile name)
  (ws/ "etc/zcpkg" name))

(current-hash-predicate
 (λ (h)
   (define topic (hash-set h 'topic 'none))
   (cond [(eq? topic 'none) #t]
         [(eq? topic 'debug)
          (ZCPKG_VERBOSE)])))

(define confirmation-answer?
  (or/c 'yes 'no 'always 'ask))

(define (user-consents? answer)
  (or (eq? answer 'yes)
      (eq? answer 'always)))

; This macro defines a reloadable configuration space derived from
; the workspace directory.

(define-syntax (define-configuration stx)
  (define (suffix-id suffix stxlist)
    (stx-map (λ (s) (format-id s "~a/~a" s suffix))
             stxlist))

  (syntax-parse stx
    [(_ [name:id cnt:expr default:expr str:expr] ...)
     (with-syntax ([(loader ...)   (suffix-id "load" #'(name ...))]
                   [(get-flag-spec ...) (suffix-id "make-flag-spec" #'(name ...))])
       #'(begin (begin (provide loader name get-flag-spec)
                       (define (loader) (load-setting 'name make-workspace-rcfile default))
                       (define (get-flag-spec f)
                         (f (setting-id->cli-flag-string 'name)
                            (format "~a (Default: ~s)" str (~s default))
                            name))
                       (define name (make-setting 'name cnt (loader))))
                ...

                (provide reload-configuration!)
                (define (reload-configuration!)
                  (name (loader)) ...)

                (provide dump-configuration)
                (define (dump-configuration)
                  (make-hash (list (cons 'name name) ...)))

                (provide call-with-reloaded-configuration)
                (define (call-with-reloaded-configuration
                         #:workspace [ws (ZCPKG_WORKSPACE)] proc)
                  (parameterize ([ZCPKG_WORKSPACE ws])
                    (parameterize ([name (loader)] ...)
                      (proc))))))]))

(define-syntax-rule (within-workspace ws body ...)
  (call-with-reloaded-configuration #:workspace ws
                                    (λ () body ...)))


(define-configuration
  ; The path length budget helps the package manager decide if it should use
  ; a custom addressing scheme when installs packages in a sufficiently-nested
  ; directory.
  [ZCPKG_PATH_LENGTH_BUDGET
   exact-positive-integer?
   (case (system-type 'os)
     ; To this day, Windows defaults to a max path length of 260 characters
     ; unless the user opted into long paths via the Registry or Group
     ; Policy Editor. Settle for checking only the Registry.
     [(windows)
      (local-require file/resource)
      (define v
        (get-resource "HKEY_LOCAL_MACHINE"
                      "SYSTEM\\CurrentControlSet\\Control\\FileSystem\\LongPathsEnabled"))
      (if (or (and (bytes? v) (bytes=? v #"1"))
              (and (string? v) (string=? v "1"))
              (equal? v 1))
          1024
          260)]

     ; Not a guarentee. Inferred from limits of HFS Plus, the default
     ; file system for OSX. The actual limit may be longer.
     [(macosx) 1024]

     ; Actually system-dependent. Some systems say 4096, but I'll be conservative.
     [(unix) 1024]

     [else #f])

   "Soft maximum characters for path."]

  ; Spacetime limits on installer sandboxes
  [ZCPKG_INSTALLER_MEMORY_LIMIT_MB
   (>=/c 0)
   30
   "Installer memory quota, in megabytes."]
  [ZCPKG_INSTALLER_TIME_LIMIT_SECONDS
   (>=/c 0)
   (* 5 60)
   "Installer time limit, in seconds."]

  ; Controls network and file I/O permissions for sandboxed installers.
  [ZCPKG_INSTALLER_ALLOWED_HOSTS
   (listof string?)
   null
   "A list of strings, where each string is a host an installer may contact."]

  [ZCPKG_INSTALLER_PATH_PERMISSIONS
   (listof (list/c (or/c 'execute 'write 'delete
                      'read-bytecode 'read 'exists)
                   (or/c byte-regexp? bytes? string? path?)))
   null
   "A value for sandbox-path-permissions"]

  ; Scenario: Artifact does not have a signature. This is normal
  ; when prototyping or working with a trusted peer, so
  ; we'll prompt by default.
  [ZCPKG_TRUST_UNSIGNED
   confirmation-answer?
   'ask
   "How to handle an unsigned artifact from a catalog"]

  ; Scenario: Artifact signature cannot be verified with publisher's public key.
  ; This is more suspicious.
  [ZCPKG_TRUST_BAD_SIGNATURE
   confirmation-answer?
   'no
   "How to handle an artifact with a signature that does not match a provider's public key."]

  ; Halt when downloaded artifact does not pass integrity check
  [ZCPKG_TRUST_BAD_DIGEST
   confirmation-answer?
   'no
   "How to handle an artifact that does not pass an integrity check."]

  [ZCPKG_COLORIZE_OUTPUT
   boolean?
   #f
   "Colorize printed output"]

  [ZCPKG_VERBOSE
   boolean?
   #f
   "Show more information in program output"]

  ; Check integrity information for files created
  ; while the installer was running.
  [ZCPKG_VERIFY_POST_INSTALL_INTEGRITY
   boolean?
   #f
   "Check integrity information after installation"]

  ; Where to install packages.
  [ZCPKG_INSTALL_RELATIVE_PATH
   path-string?
   "usr/lib/racket"
   "Workspace-relative path for installed packages"]

  ; A list of catalogs to try.
  [ZCPKG_SERVICE_ENDPOINTS
   (listof (cons/c string? url-string?))
   (list (cons "default" "https://zcpkgs.com"))
   (string-append "A list of catalog pairs to try when installing "
                  "packages (e.g. ((\"catalog-name\" . \"http://example.com\") ...).")]

  [ZCPKG_USE_INSTALLER
   confirmation-answer?
   'ask
   "How to handle package installers."]

  [ZCPKG_INSTALL_DEPENDENCIES
   confirmation-answer?
   'ask
   "How to handle installing package dependencies."]

  [ZCPKG_UNINSTALL_ORPHANS
   confirmation-answer?
   'ask
   "How to handle orphaned package dependents."]

  [ZCPKG_DOWNLOAD_MAX_REDIRECTS
   exact-nonnegative-integer?
   2
   "Maximum redirects to follow when downloading an artifact"]

  ; Guarentee a cache miss when downloading artifacts, if #t.
  [ZCPKG_DOWNLOAD_IGNORE_CACHE
   boolean?
   #f
   "If set, ignore the download cache."])
