#lang racket/base

(require idiocket/contract
         idiocket/match
         "setting.rkt"
         "url.rkt"
         "workspace.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx))

(define (make-workspace-rcfile name)
  (ws/ "etc/zcpkg" name))

; This macro defines a reloadable configuration space derived from
; the workspace directory.

(define-syntax (define-configuration stx)
  (syntax-parse stx
    [(_ [name:id cnt:expr default:expr] ...)
     (with-syntax ([(loader ...)
                    (stx-map (λ (s) (format-id s "~a/load" s))
                             #'(name ...))])
       #'(begin (begin (provide loader name)
                       (define (loader) (load-setting 'name make-workspace-rcfile default))
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

     [else #f])]

  ; Spacetime limits on installer sandboxes
  [ZCPKG_INSTALLER_MEMORY_LIMIT_MB (>=/c 0) 30]
  [ZCPKG_INSTALLER_TIME_LIMIT_SECONDS (>=/c 0) (* 5 60)]

  ; Controls network and file I/O permissions for sandboxed installers.
  [ZCPKG_INSTALLER_ALLOWED_HOSTS (listof string?) null]
  [ZCPKG_INSTALLER_PATH_PERMISSIONS
   (listof (list/c (or/c 'execute 'write 'delete
                      'read-bytecode 'read 'exists)
                   (or/c byte-regexp? bytes? string? path?)))
   null]

  ; Scenario: Artifact does not have a signature. This is normal
  ; when prototyping or working with a trusted peer, so
  ; we'll prompt by default.
  [ZCPKG_ON_UNSIGNED_ARTIFACT (or/c 'allow 'fail 'ask) 'ask]

  ; Scenario: Artifact signature cannot be verified with publisher's public key.
  ; This is more suspicious.
  [ZCPKG_ON_SIGNATURE_MISMATCH (or/c 'allow 'fail 'ask) 'fail]

  ; Halt when downloaded artifact does not pass integrity check
  [ZCPKG_ON_TAMPERED_ARTIFACT (or/c 'allow 'fail 'ask) 'fail]

  ; Colorize printed output
  [ZCPKG_COLORIZE_OUTPUT boolean? #f]

  ; Print 'debug level logs
  [ZCPKG_VERBOSE boolean? #f]

  ; Include installer I/O operations on 'debug level.  Does not imply
  ; ZCPKG_VERBOSE becaue log receivers are considered separate than
  ; the desire to print the debug level.
  [ZCPKG_LOG_INSTALLER_IO boolean? #f]

  ; Check integrity information for files created
  ; while the installer was running.
  [ZCPKG_VERIFY_POST_INSTALL_INTEGRITY boolean? #f]

  ; Where to install packages.
  [ZCPKG_INSTALL_RELATIVE_PATH path-string? "opt"]

  ; A list of catalogs to try.
  [ZCPKG_SERVICE_ENDPOINTS (listof (cons/c string? url-string?))
                           (list (cons "default" "https://zcpkgs.com"))]

  ; The maximum number of redirects to follow when downloading an artifact.
  [ZCPKG_DOWNLOAD_MAX_REDIRECTS exact-nonnegative-integer? 2]

  ; Guarentee a cache miss when downloading artifacts, if #t.
  [ZCPKG_DOWNLOAD_IGNORE_CACHE boolean? #f])
