#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/pretty
         (only-in racket/tcp listen-port-number?)
         "config.rkt"
         "contract.rkt"
         "setting.rkt"
         "string.rkt"
         "url.rkt"
         "workspace.rkt")

(define (make-zcpkg-settings-namespace)
  (define ns (make-base-namespace))
  (for ([(k v) (in-hash ZCPKG_SETTINGS)])
    (namespace-set-variable-value! k v #t ns #t))
  (namespace-set-variable-value! 'save! save-zcpkg-settings! #t ns #t)
  (namespace-set-variable-value! 'dump
                                 (λ () (pretty-write #:newline? #t (dump-zcpkg-settings)))
                                 #t ns #t)
  ns)

(define (get-zcpkg-settings-path)
  (build-workspace-path "etc/zcpkg.rkt"))

(define (load-zcpkg-settings!)
  (define path (get-zcpkg-settings-path))
  (define lookup
    (if (file-exists? path)
        (load-config path)
        (make-config-closure (hasheq) null)))

  ; setting.rkt has its own protocol for protecting values. This
  ; procedure disables checks for the config closure that are
  ; redundant in this context.
  (current-setting-value-lookup
   (λ (k) (if (eq? k READ_ORDER)
              (lookup READ_ORDER)
              (lookup k any/c (void))))))

(define (get-zcpkg-setting . args)
  (apply hash-ref ZCPKG_SETTINGS args))

(define (get-zcpkg-setting-value . args)
  ((apply get-zcpkg-setting args)))

(define (change-zcpkg-setting! k v)
  ((get-zcpkg-setting k) v))

(define (dump-zcpkg-settings)
  (for/hash ([(k v) (in-hash ZCPKG_SETTINGS)])
    (values k (v))))

(define (save-zcpkg-settings!)
  (save-config!
   (make-config-closure (dump-zcpkg-settings)
                        ((current-setting-value-lookup) READ_ORDER))
   (get-zcpkg-settings-path)))


(define (configure-zcpkg! h)
  (for/hash ([(k v) (in-hash ZCPKG_SETTINGS)])
    (values k (v (hash-ref h k (v))))))

; Reset each setting's override value
(define (reset-zcpkg-setting-overrides!)
  (for ([(k v) (in-hash ZCPKG_SETTINGS)])
    (v (void))))

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

  {ZCPKG_SANDBOX_MEMORY_LIMIT_MB
   "-M"
   ("Total memory quota for a sandbox" "mibibytes")
   (>=/c 0)
   30}

  {ZCPKG_SANDBOX_EVAL_MEMORY_LIMIT_MB
   "-e"
   ("Memory quota for each sandboxed expression" "mibibytes")
   (>=/c 0)
   10}

  {ZCPKG_SANDBOX_EVAL_TIME_LIMIT_SECONDS
   "-S"
   ("Time limit for each sandboxed expression" "seconds")
   (>=/c 0)
   (* 5 60)}

  ; Controls network and file I/O permissions for sandboxed installers.
  {ZCPKG_SANDBOX_NETWORK_PERMISSIONS
   "-N"
   ("Regex patterns permissions" "string-list")
   (list/c (or/c #f string?)
           (or/c #f string?)
           (or/c #f string?)
           (or/c #f string?))
   '(#f #f #f #f)}

  {ZCPKG_MATCH_RACKET_MODULES
   "-r"
   ("Match against .rkt, .ss, .scrbl, and .rktd.")
   boolean?
   #f}

  {ZCPKG_MATCH_COMPILED_RACKET
   "-b"
   ("Match against .zo and .dep.")
   boolean?
   #f}

  {ZCPKG_BUNDLE_FOR_SERVER
   "-s"
   ("Bundle package into server directory.")
   boolean?
   #f}

  {ZCPKG_EDITION
   "-e"
   ("The name of the edition. If not set, use existing name." "name")
   (or/c #f symbol? name-string?)
   #f}

  {ZCPKG_REVISION_NUMBER
   "-r"
   ("Set revision number" "revision-number")
   (or/c #f exact-nonnegative-integer?)
   #f}

  {ZCPKG_SANDBOX_PATH_PERMISSIONS
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

  {ZCPKG_TRUST_UNVERIFIED_HOST
   #f
   ("Trust servers that do not have a valid certificate.")
   boolean?
   #f}

  ; Halt when downloaded artifact does not pass integrity check
  {ZCPKG_TRUST_BAD_DIGEST
   "-D"
   ("Trust artifacts that don't pass an integrity check. Implies -U.")
   boolean?
   #f}

  {ZCPKG_FASL_OUTPUT
   "-F"
   ("Use FASL program output")
   boolean?
   #f}

  {ZCPKG_READER_FRIENDLY_OUTPUT
   "-R"
   ("Use (read)able program output")
   boolean?
   #f}

  {ZCPKG_VERBOSE
   "-v"
   ("Show more information in program output")
   boolean?
   #f}

  {ZCPKG_PORT
   "-p"
   ("Set listen port" "port-number")
   listen-port-number?
   8080}

  {ZCPKG_PRIVATE_KEY_PATH
   "-q"
   ("The location of a private key" "path")
   (or/c #f path-string?)
   #f}

  ; Where to install packages.
  {ZCPKG_INSTALL_RELATIVE_PATH
   "-I"
   ("Workspace-relative path for installed packages" "relative-path-string")
   (and/c path-string? (not/c complete-path?))
   "usr/lib/racket"}

  ; Where to place launchers
  {ZCPKG_LAUNCHER_RELATIVE_PATH
   "-L"
   ("Workspace-relative path for launchers" "relative-path-string")
   (and/c path-string? (not/c complete-path?))
   "bin"}

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

  {ZCPKG_LINK
   "-l"
   ("When installing a package on the filesystem, create a symlink to the source directory.")
   boolean?
   #f}

  {ZCPKG_CONSENT
   "-y"
   ("Proceed with operation")
   boolean?
   #f}

  {ZCPKG_LEAVE_ORPHANS
   "-o"
   ("Leave orphaned packages on the system")
   boolean?
   #f}

  {ZCPKG_DOWNLOAD_MAX_REDIRECTS
   "-r"
   ("Maximum redirects to follow when downloading an artifact" "exact-nonnegative-integer")
   exact-nonnegative-integer?
   2}

  ; Guarentee a cache miss when downloading artifacts, if #t.
  {ZCPKG_IGNORE_DOWNLOAD_CACHE
   #f
   ("If set, ignore the download cache.")
   boolean?
   #f})
