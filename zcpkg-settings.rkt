#lang racket/base

(provide (all-defined-out))

(require racket/match
         "config.rkt"
         "contract.rkt"
         "setting.rkt"
         "url.rkt"
         "workspace.rkt")

(define current-zcpkg-config (make-parameter #f))

(define (get-zcpkg-settings-path)
  (build-workspace-path "etc/zcpkg.rktd"))

(define (load-zcpkg-settings!)
  (define path (get-zcpkg-settings-path))
  (define lookup
    (if (file-exists? path)
        (load-config path)
        (make-config-closure (hasheq) null)))

  (current-setting-value-lookup
   (λ (k) (lookup k any/c (void))))

  (define (controller . args)
    (match args
      [(list 'get-path)
       path]

      [(list 'get-setting r ...)
       (apply hash-ref ZCPKG_SETTINGS r)]

      [(list 'get-value r ...)
       ((apply controller 'get-setting r))]

      [(list 'set k v)
       ((controller 'get-setting k) v)]

      [(list 'dump)
       (for/hash ([(k v) (in-hash ZCPKG_SETTINGS)])
         (values k (v)))]

      [(list 'save!)
       (save-config!
        (make-config-closure (controller 'dump)
                             (lookup READ_ORDER))
        path)]))

  (current-zcpkg-config controller)
  controller)

; The path length budget helps the package manager decide if it should use
; a custom addressing scheme when installs packages in a sufficiently-nested
; directory.
(define-setting-group ZCPKG_SETTINGS
  {ZCPKG_PATH_LENGTH_BUDGET
   "-P"
  ("Maximum characters for path." "num-chars")
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
    [else #f])}

  {ZCPKG_INSTALLER_MEMORY_LIMIT_MB
   "-M"
   ("Installer memory quota" "mibibytes")
   (>=/c 0)
   30}

  {ZCPKG_INSTALLER_TIME_LIMIT_SECONDS
   "-S"
   ("Installer time limit" "seconds")
   (>=/c 0)
   (* 5 60)}

  ; Controls network and file I/O permissions for sandboxed installers.
  {ZCPKG_INSTALLER_ALLOWED_HOSTS
   "-H"
   ("Hosts an installer may contact." "string-list")
   (listof string?)
   null}

  {ZCPKG_INSTALLER_PATH_PERMISSIONS
   "-P"
   ("A value for sandbox-path-permissions" "racket-value")
   (listof (list/c (or/c 'execute 'write 'delete
                         'read-bytecode 'read 'exists)
                   (or/c byte-regexp? bytes? string? path?)))
   null}

  ; Scenario: Artifact does not have a signature. This is normal
  ; when prototyping or working with a trusted peer, so
  ; we'll prompt by default.
  {ZCPKG_TRUST_UNSIGNED
   "-U"
   ("Trust unsigned packages")
   boolean?
   #f}


  ; Scenario: Artifact signature cannot be verified with publisher's public key.
  ; This is more suspicious.
  {ZCPKG_TRUST_BAD_SIGNATURE
   #f
   ("Trust signatures that don't match provider's public key")
   boolean?
   #f}

; Halt when downloaded artifact does not pass integrity check
  {ZCPKG_TRUST_BAD_DIGEST
   "-D"
   ("Trust artifacts that don't pass an integrity check. Implies -U.")
   boolean?
   #f}

  {ZCPKG_COLORIZE_OUTPUT
   "-c"
   ("Colorize printed output")
   boolean?
   #f}

  {ZCPKG_VERBOSE
   "-v"
   ("Show more information in program output")
   boolean?
   #f}

  ; Where to install packages.
  {ZCPKG_INSTALL_RELATIVE_PATH
   "-I"
   ("Workspace-relative path for installed packages" "relative-path-string")
   (and/c path-string? (not/c complete-path?))
   "usr/lib/racket"}

  ; A list of catalogs to try.
  {ZCPKG_SERVICE_ENDPOINTS
   "-E"
   ("Catalogs to try when downloading packages"
    "list")
   (listof (cons/c string? url-string?))
   (list (cons "default" "https://zcpkgs.com"))}

  {ZCPKG_INSTALL_ORPHAN
   "-o"
   ("When installing a package, do not install dependencies.")
   boolean?
   #f}

  {ZCPKG_INSTALL_CONSENT
   "-y"
   ("Proceed with installation")
   boolean?
   #f}

  {ZCPKG_LEAVE_ORPHANS
   "-o"
   ("Leave orphaned packages on the system")
   boolean?
   #t}

  {ZCPKG_DOWNLOAD_MAX_REDIRECTS
   "-r"
   ("Maximum redirects to follow when downloading an artifact" "exact-nonnegative-integer")
   exact-nonnegative-integer?
   2}

  ; Guarentee a cache miss when downloading artifacts, if #t.
  {ZCPKG_DOWNLOAD_IGNORE_CACHE
   "-D"
   ("If set, ignore the download cache.")
   boolean?
   #f})
