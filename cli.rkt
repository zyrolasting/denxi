#lang racket/base

(require racket/class
         racket/cmdline
         racket/path
         racket/pretty
         racket/list
         racket/match
         racket/os
         racket/path
         racket/place
         racket/port
         racket/runtime-path
         racket/sequence
         racket/set
         racket/vector
         (for-syntax racket/base)
         "archiving.rkt"
         "capture.rkt"
         "config.rkt"
         "contract.rkt"
         "zcpkg-query.rkt"
         "download.rkt"
         "file.rkt"
         "format.rkt"
         "message.rkt"
         "output.rkt"
         "resolve.rkt"
         "setting.rkt"
         "setup.rkt"
         "server.rkt"
         "string.rkt"
         "team.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-messages.rkt"
         "zcpkg-settings.rkt"
         "zcpkg-worker.rkt")


(module+ main
  (define-values (exit-code output) (entry-point))
  (exit exit-code))

(define-runtime-path worker.rkt "zcpkg-worker.rkt")

(define (zcpkg-start-team!)
  (define team
    (new team%
         [make-place (λ () (dynamic-place worker.rkt 'main))]))

  ; Give each worker the same configuration
  (send team broadcast!
        ($start (workspace-directory)
                (dump-zcpkg-settings)))

  team)


(define (process-jobs team tasks)
  (define output (send team send-assigned-tasks! tasks))
  (sequence-for-each write-output (in-list output))
  output)

(define (zcpkg-stop-team! team)
  (send team stop!))

(define current-cli-continuation (make-parameter #f))

(define (halt exit-code)
  ((current-cli-continuation) exit-code))

; Keep seperate for functional tests.
(define (entry-point #:reload-config? [reload-config? #t] [args (current-command-line-arguments)])
  (when reload-config?
    (reset-zcpkg-setting-overrides!)
    (load-zcpkg-settings!))

  (when (show-workspace-envvar-error?)
    (write-output ($invalid-workspace-envvar)))

  (define exit-code
    (call/cc
     (λ (k)
       (parameterize ([current-cli-continuation k])
         (top-level-cli args)))))

  (values exit-code (get-output #:reset? #t)))

(define (top-level-cli args)
  (run-command-line
   #:program "zcpkg"
   #:arg-help-strings '("action" "args")
   #:args args
   #:flags
   (settings->flag-specs
    ZCPKG_COLORIZE_OUTPUT
    ZCPKG_FASL_OUTPUT
    ZCPKG_READER_FRIENDLY_OUTPUT
    ZCPKG_VERBOSE)
   (λ (flags action . args)
     (define proc
       (match action
         ["install" install-command]
         ["uninstall" uninstall-command]
         ["new" new-command]
         ["show" show-command]
         ["link" link-command]
         ["setup" setup-command]
         ["config" config-command]
         ["capture" capture-command]
         ["restore" restore-command]
         ["diff" diff-command]
         ["sandbox" sandbox-command]
         ["serve" serve-command]
         ["chver" chver-command]
         ["bundle" bundle-command]
         [_ (write-output ($unrecognized-command action))
            (λ _ 1)]))
     (proc args))

#<<EOF
<action> is one of
  install    Install packages
  uninstall  Uninstall packages
  new        Create a new package
  show       Print helpful information
  link       Create a symlink to an installed package
  setup      Integrate a package
  config     Configure the package manager
  capture    Capture workspace
  restore    Restore workspace
  diff       Compare workspace to capture
  sandbox    Start sandboxed REPL for package's setup module.
  serve      Serve package artifacts
  chver      Change package version
  bundle     Prepare package for distribution

EOF
))


(define (install-command args)
  (define (review-work package-sources sow)
    (write-output ($review-installation-work sow package-sources))
    (halt 0))

  (define (do-work sow)
    (define team (zcpkg-start-team!))
    (dynamic-wind
      void
      (λ ()
        ; Installation tasks come strictly before setup tasks.
        (process-jobs team
                      (for/list ([(url-or-path infos) (in-hash sow)])
                        ($install-package (car infos) (cdr infos) url-or-path)))

        (process-jobs
         (send team send-assigned-tasks!
               (for/list ([infos (in-hash-values sow)])
                 ($setup-package (car infos) (cdr infos) null)))))
      (λ () (halt (zcpkg-stop-team! team)))))

  (run-command-line
   #:program "install"
   #:args args
   #:arg-help-strings '("package-source")
   #:flags
   (settings->flag-specs
    ZCPKG_SERVICE_ENDPOINTS
    ZCPKG_DOWNLOAD_MAX_REDIRECTS
    ZCPKG_INSTALL_RELATIVE_PATH
    ZCPKG_LAUNCHER_RELATIVE_PATH
    ZCPKG_TRUST_BAD_DIGEST
    ZCPKG_TRUST_BAD_SIGNATURE
    ZCPKG_TRUST_UNSIGNED
    ZCPKG_INSTALL_ORPHAN
    ZCPKG_CONSENT
    ZCPKG_LINK
    ZCPKG_IGNORE_DOWNLOAD_CACHE)

   (λ (flags . package-sources)
     (define sow (find-scope-of-work package-sources))

     (when (null? package-sources)
       (write-output ($no-package-sources))
       (halt 1))

     (if (ZCPKG_CONSENT)
         (do-work sow)
         (review-work package-sources sow)))))


(define (capture-command args)
  (run-command-line
   #:program "capture"
   #:flags
   (settings->flag-specs
    ZCPKG_MATCH_RACKET_MODULES
    ZCPKG_MATCH_COMPILED_RACKET)
   #:args args
   #:arg-help-strings '("pregexp-strings")
   (λ (flags . file-patterns)
     (define consider-modules
       (if (ZCPKG_MATCH_RACKET_MODULES)
           (cons (make-extension-pattern-string "rkt" "ss" "scrbl" "rktd")
                 file-patterns)
           file-patterns))

     (define consider-compiled
       (if (ZCPKG_MATCH_COMPILED_RACKET)
           (cons (make-extension-pattern-string "dep" "zo")
                 consider-modules)
           consider-modules))

     (write-output ($on-workspace-capture (capture-workspace consider-compiled)))
     (halt 0))))


(define (restore-command args)
  (define (do-work capture-file delete-commands configure-commands install-commands)
    (for ([cmd (in-list delete-commands)])
      (define target ($restore-delete-file-path cmd))
      (when (file-exists? target)
        (delete-file target))
      (when (directory-exists? target)
        (delete-directory/files target)))

    (reset-zcpkg-setting-overrides!)
    (for ([cmd (in-list configure-commands)])
      (change-zcpkg-setting! ($restore-config-name cmd)
                             ($restore-config-value cmd)))

    (save-zcpkg-settings!)
    (write-output ($after-write (get-zcpkg-settings-path)))

    (entry-point (list->vector
                  `("install"
                    ,(setting->short-flag ZCPKG_CONSENT)
                    . ,(map $restore-package-query install-commands)))))

  (define (show-work capture-file delete-commands configure-commands install-commands)
    (write-output
     ($review-restoration-work capture-file
                               (workspace-directory)))

    (sequence-for-each write-output
                       (sequence-append (in-list delete-commands)
                                        (in-list configure-commands)
                                        (in-list install-commands)))

    (halt 0))

  (run-command-line
   #:program "restore"
   #:args args
   #:flags
   (settings->flag-specs ZCPKG_CONSENT)
   #:arg-help-strings '("capture-file")
   (λ (flags capture-file)
     (define lookup (load-config (string->path capture-file)))
     (define diff (diff-workspace lookup))
     (define config (lookup 'config))
     (define packages (lookup 'packages))

     (define config-commands
       (for/list ([(k v) (in-hash config)])
         ($restore-config k v)))

     (define install-commands
       (for/list ([query (in-list packages)])
         ($restore-package query)))

     (define delete-commands
       (for/fold ([l null])
                 ([m (in-list diff)])
         (define path
           (cond [($diff-different-file? m)
                  ($diff-different-file-path m)]
                 [($diff-extra-file? m)
                  ($diff-extra-file-path m)]
                 [else #f]))
         (if path
             (cons ($restore-delete-file path) l)
             l)))

     ((if (ZCPKG_CONSENT) do-work show-work)
      capture-file
      delete-commands
      config-commands
      install-commands))))


(define (diff-command args)
  (run-command-line
   #:program "diff"
   #:args args
   #:arg-help-strings '("capture-file-path")
   (λ (flags capture-file)
     (define lookup (load-config (string->path capture-file)))
     (for ([m (in-list (diff-workspace lookup))])
       (write-output m))
     (halt 0))))


(define (link-command args)
  (run-command-line
   #:program "link"
   #:args args
   #:arg-help-strings '("nss" "link-path")
   (λ (flags source link-path)
     (define seq (search-zcpkg-infos source (in-installed-info)))
     (if (= (sequence-length seq) 0)
         (begin
           (write-output ($link-command-no-package source))
           (halt 1))
         (begin
           (make-file-or-directory-link
            (zcpkg-info->install-path
             (sequence-ref seq 0))
            link-path)
           (halt 0))))))


(define (setup-command args)
  (define (do-work info exprs)
    (define sow (find-scope-of-work info))
    (define team (zcpkg-start-team!))
    (dynamic-wind
      void
      (λ ()
        (process-jobs team
                      (for/list ([infos (in-hash-values sow)])
                        ($setup-package (car infos) (cdr infos) null))))
      (λ () (halt (zcpkg-stop-team! team)))))

  (run-command-line
   #:program "setup"
   #:args args
   #:flags
   (settings->flag-specs
    ZCPKG_LAUNCHER_RELATIVE_PATH)
   #:arg-help-strings '("query" "expr")
   (λ (flags query . exprs)
     (define infos (search-zcpkg-infos query (in-installed-info)))
     (if (= (sequence-length infos) 0)
         (begin (write-output ($setup-command-no-package query))
                (halt 1))
         (do-work (sequence-ref infos 0) exprs)))))


(define (config-command args)
  (define (make-fail-thunk str)
    (λ ()
      (write-output ($config-command-nonexistant-setting str))
      (halt 1)))

  (run-command-line
   #:program "config"
   #:args args
   #:arg-help-strings '("action" "args")

   (λ (flags action . args)
     (match action
       ["get"
        (run-command-line #:args args
                          #:program "config-get"
                          #:arg-help-strings '("key")
                          (λ (flags key)
                            (pretty-write #:newline? #t
                                          (get-zcpkg-setting-value
                                           (string->symbol key)
                                           (make-fail-thunk key)))
                            (halt 0)))]
       ["dump"
        (run-command-line #:args args
                          #:program "config-dump"
                          #:arg-help-strings '()
                          (λ (flags)
                            (pretty-write #:newline? #t
                                          (dump-zcpkg-settings))
                            (halt 0)))]

       ["set"
        (run-command-line #:args args
                          #:program "config-set"
                          #:arg-help-strings '("key" "value")
                          (λ (flags key value)
                            (define sym (string->symbol key))
                            (define picked-setting
                              (get-zcpkg-setting sym (make-fail-thunk key)))

                            (define to-write
                              (with-handlers ([exn:fail?
                                               (λ (e)
                                                 (write-output
                                                  ($reject-user-setting
                                                   sym
                                                   value
                                                   (cadr (regexp-match #px"expected:\\s+([^\n]+)"
                                                                       (exn-message e)))))
                                                 (halt 0))])
                                (picked-setting (read (open-input-string value)))
                                (picked-setting)))

                            (save-zcpkg-settings!)
                            (write-output ($after-write (get-zcpkg-settings-path)))
                            (halt 0)))]

       ["repl"
        (define (start-repl)
          (with-handlers ([exn:fail:contract?
                           (λ (e)
                             (define m (regexp-match #px"expected:\\s+[^\n]+" (exn-message e)))
                             (if (null? m)
                                 (raise e)
                                 (begin (displayln (car m))
                                        (start-repl))))]
                          [exn:fail?
                           (λ (e)
                             ((error-display-handler) e)
                             (start-repl))]
                          [exn:break?
                           (λ (e)
                             (displayln "bye"))])
            (parameterize ([current-namespace (make-zcpkg-settings-namespace)])
              (read-eval-print-loop))))

        (run-command-line #:args args
                          #:program "config-repl"
                          #:arg-help-strings null
                          (λ (flags) (start-repl)))]

       [_ (write-output ($unrecognized-command action))
          (halt 1)]))

   #<<EOF
<action> is one of
  set   Set a new value for a setting
  get   Get the current value of a setting
  dump  Write a hash table of all settings
  repl  Start a REPL to edit the rcfile

EOF
))


(define (chver-command args)
  (run-command-line
   #:program "chver"
   #:arg-help-strings '("package-path" "revision-names")
   #:flags
   (settings->flag-specs
    ZCPKG_EDITION
    ZCPKG_REVISION_NUMBER)
   #:args args
   (λ (flags package-path-string . revision-names)
     (define info
       (with-handlers ([exn:fail:filesystem?
                        (λ (e)
                          (write-output ($package-directory-has-unreadable-info package-path-string))
                          (halt 1))])
         (read-zcpkg-info-from-directory package-path-string)))

     (define (assert-valid-info name info)
       (define errors (validate-zcpkg-info info))
       (unless (null? errors)
         (write-output ($chver-command-bad-info name errors))
         (halt 1))
       info)

     (define new-info
       (assert-valid-info
        "new"
        (struct-copy zcpkg-info (assert-valid-info "original" info)
                     [edition-name (if (ZCPKG_EDITION)
                                       (~a (ZCPKG_EDITION))
                                       (zcpkg-info-edition-name info))]
                     [revision-number (if (ZCPKG_REVISION_NUMBER)
                                          (ZCPKG_REVISION_NUMBER)
                                          (add1 (zcpkg-info-revision-number info)))]
                     [revision-names revision-names])))

     (call-with-output-file
       #:exists 'truncate/replace
       (build-path package-path-string
                   CONVENTIONAL_PACKAGE_INFO_FILE_NAME)
       (λ (o) (write-zcpkg-info new-info o)))

     (write-output ($chver-command-updated-info info new-info))
     (halt 0))))

(define (bundle-command args)
  (run-command-line
   #:program "bundle"
   #:arg-help-strings '("package-path" "pregexp-pattern-strings")
   #:flags
   (settings->flag-specs ZCPKG_PRIVATE_KEY_PATH
                         ZCPKG_BUNDLE_FOR_SERVER
                         ZCPKG_MATCH_RACKET_MODULES)
   #:args args
   (λ (flags package-path-string . pattern-strings)
     (define info
       (with-handlers ([exn:fail:filesystem?
                        (λ (e)
                          (write-output ($package-directory-has-unreadable-info package-path-string))
                          (halt 1))])
         (read-zcpkg-info-from-directory package-path-string)))

     ; Detach package from filesystem, leaving it only able to depend
     ; on URNs.
     (define dependencies/nonlocalized
       (map (λ (dep)
              (define variant (source->variant dep package-path-string))
              (cond [(and (path? variant)
                          (directory-exists? variant))
                     (zcpkg-query->string (coerce-zcpkg-query (read-zcpkg-info-from-directory variant)))]
                    [(zcpkg-query? variant)
                     (zcpkg-query->string variant)]
                    [else (raise-user-error
                           (format (~a "Cannot bundle a package with a dependency on ~s.~n"
                                       "Use a path or a package query.~n")
                                   dep))]))
            (zcpkg-info-dependencies info)))

     (define archive-directory
       (if (ZCPKG_BUNDLE_FOR_SERVER)
           (let ([d (zcpkg-info->public-file-path info)])
             (make-directory* d)
             d)
           (current-directory)))

     (define archive-file
       (build-path archive-directory "archive.tgz"))
     (define metadata-file
       (build-path archive-directory CONVENTIONAL_PACKAGE_INFO_FILE_NAME))

     (define archive
       (parameterize ([current-directory package-path-string])
         (define archive-files
           (sequence->list
            (in-matching-files
             (map pregexp (if (ZCPKG_MATCH_RACKET_MODULES)
                              (cons "\\.(ss|rkt|rktd|scrbl)$" pattern-strings)
                              pattern-strings))
             (current-directory))))

         (when (null? archive-files)
           (write-output ($no-files-match))
           (halt 1))

         (pack archive-file archive-files)))

     (define-values (exit-code/digest digest) (make-digest archive))
     (unless (= exit-code/digest 0)
       (write-output ($cannot-make-bundle-digest exit-code/digest))
       (halt 1))

     (define-values (exit-code/signature signature)
       (if (ZCPKG_PRIVATE_KEY_PATH)
           (make-signature digest (ZCPKG_PRIVATE_KEY_PATH))
           (values 0 #f)))

     (unless (= exit-code/signature 0)
       (write-output ($cannot-make-bundle-signature exit-code/signature))
       (halt 1))

     (save-config! (make-config-closure
                    (zcpkg-info->hash
                     (struct-copy zcpkg-info info
                                  [dependencies dependencies/nonlocalized]
                                  [signature signature]
                                  [integrity digest]))
                    null)
                   metadata-file)

     (define links-made
       (if (ZCPKG_BUNDLE_FOR_SERVER)
           (make-zcpkg-revision-links info #:target archive-directory)
           null))

     (for ([created (in-list (append (list archive-file
                                           metadata-file)
                                     links-made))])
       (write-output ($after-write (path->string created))))

     (halt 0))))



(define (new-command args)
  (define (make-file dir file-name proc)
    (call-with-output-file (build-path dir file-name)
      (λ (o) (parameterize ([current-output-port o])
               (proc)))))


  (define (make-package name)
    (make-directory name)

    (make-file name CONVENTIONAL_PACKAGE_INFO_FILE_NAME
               (λ ()
                 (define defn
                   `((package . ,name)
                     (description . "Describe this package in a sentence.")
                     (tags . ())
                     (home-page . "https://example.com")
                     (edition . "draft")
                     (revision-number . 0)
                     (revision-names . ())
                     (provider . ,(gethostname))
                     (setup-module . "setup.rkt")
                     (dependencies . ())
                     (launchers . ())
                     (racket-versions . (("6.0" . ,(version))))))

                 (write-config
                  (make-immutable-hasheq defn)
                  (map car defn)
                  (current-output-port))))

    (make-file name "setup.rkt"
               (λ ()
                 (display-lines
                  '("#lang racket/base"
                    ""
                    "; This module is for setting up dependencies in userspace outside of Racket,"
                    "; like an isolated Python installation, a C library, or even another Racket"
                    "; installation."
                    ";"
                    "; For security, `zcpkg` asks the user to run (show-help) in a zero-trust sandbox"
                    "; to learn what EXACTLY your package will do to their system if they consent "
                    "; to extra automated setup."
                    ";"
                    "; Write (show-help) to explain the bindings in this module, and the permissions"
                    "; you need for them to work."
                    ";"
                    "; To build trust:"
                    ";   1. Ask for as little as possible."
                    ";   2. Be specific. \"I need write permission for /etc\" is too broad,"
                    ";      and that can come off as suspicious."
                    ";   3. Do exactly what you say you will do, and nothing else."
                    ";"
                    "; Finally, be mindful that other versions of your package may have"
                    "; already modified the user's system. Look for evidence of your impact,"
                    "; and react accordingly."
                    ""
                    "(define (show-help)"
                    "  (displayln \"I need write permission for...\"))")))))

  (run-command-line
   #:program "new"
   #:arg-help-strings '("package-name" "package-names")
   #:args args
   (λ (flags name . names)
     (define targets (cons name names))
     (define existing
       (filter-map (λ (t)
                     (and (or (file-exists? t)
                              (directory-exists? t)
                              (link-exists? t))
                          t))
                   targets))

     (unless (null? existing)
       (write-output ($new-package-conflict existing))
       (halt 1))

     (for ([t (in-list targets)])
       (make-package t))

     (halt 0))))


(define (serve-command args)
  (run-command-line
   #:program "serve"
   #:arg-help-strings null
   #:flags
   (settings->flag-specs
    ZCPKG_PORT)
   #:args args
   (λ (flags)
     (define stop (start-server #:port (ZCPKG_PORT)))
     (with-handlers ([exn:break?
                      (λ (e)
                        (write-output ($on-server-break))
                        (halt 0))])
       (dynamic-wind void
                     (λ ()
                       (write-output ($on-server-up (format "[::]:~a" (ZCPKG_PORT))))
                       (sync/enable-break never-evt))
                     stop)))))


(define (sandbox-command args)
  (run-command-line
   #:program "sandbox"
   #:arg-help-strings '("query")

   #:flags
   (settings->flag-specs
    ZCPKG_SANDBOX_MEMORY_LIMIT_MB
    ZCPKG_SANDBOX_EVAL_MEMORY_LIMIT_MB
    ZCPKG_SANDBOX_EVAL_TIME_LIMIT_SECONDS
    ZCPKG_SANDBOX_PATH_PERMISSIONS)

   #:args args
   (λ (flags query)
     (enter-setup-module (find-exactly-one-info query)))))


(define (show-command args)
  (run-command-line
   #:args args
   #:program "show"
   #:arg-help-strings '("what")
   (λ (flags what)
     (match what
       ["workspace"
        (displayln (workspace-directory))
        (halt 0)]
       ["installed"
        (for ([info (in-installed-info)])
          (printf "~a: ~a~n"
                  (format-zcpkg-info info)
                  (zcpkg-info->install-path info)))
        (halt 0)]
       [_
        (write-output ($unrecognized-command what))
        (halt 1)]))
   #<<EOF
where <what> is one of
  workspace  The current target workspace directory
  installed  A list of installed packages, as queries

EOF
))

(define (uninstall-command args)
  (run-command-line
   #:program "uninstall"
   #:arg-help-strings '("queries")
   #:flags
   (settings->flag-specs
    ZCPKG_LEAVE_ORPHANS
    ZCPKG_CONSENT)
   #:args args
   (λ (flags . queries)
     (define to-uninstall (mutable-set))
     (define will-be-orphaned (mutable-set))

     ; Determine impact to the system
     (for ([query (in-list queries)])
       (define info (find-exactly-one-info query))
       (define target-install-path (zcpkg-info->install-path info))
       (set-add! to-uninstall info)

       (for ([install-path (in-installed-package-paths)])
         (define link-path (build-dependency-path install-path info))
         (when (and (link-exists? link-path)
                    (equal? (file-or-directory-identity link-path)
                            (file-or-directory-identity target-install-path)))
           (define orphan-info (read-zcpkg-info-from-directory install-path))
           (set-add! will-be-orphaned orphan-info)
           (unless (ZCPKG_LEAVE_ORPHANS)
             (set-add! to-uninstall orphan-info)))))

     (if (ZCPKG_CONSENT)
         (for ([info (in-set to-uninstall)])
           (define install-path (zcpkg-info->install-path info))
           (define deleted-revision-links (delete-zcpkg-revision-links info))
           (define deleted-launchers (delete-launchers info))
           (delete-directory/files/empty-parents install-path)
           (sequence-for-each (λ (path) (write-output ($after-delete path)))
                              (sequence-append (in-list deleted-revision-links)
                                               (in-list deleted-launchers)
                                               (in-value install-path))))
         (write-output ($review-uninstallation-work
                        (sequence->list (in-mutable-set to-uninstall)))))

     (halt 0))))



; Base bindings follow

(define (run-command-line #:program program
                          #:flags [flags null]
                          #:args [args (current-command-line-arguments)]
                          #:arg-help-strings arg-help-strings
                          #:suffix-is-index? [suffix-is-index? #t]
                          handle-arguments
                          [help-suffix ""])
  (define argv
    (if (list? args)
        (list->vector args)
        args))

  (define help-requested?
    (or (vector-member "-h" argv)
        (vector-member "--help" argv)))

  (with-handlers ([exn:fail:user?
                   ; parse-command-line barfs a sometimes unhelpful exception
                   ; when an argument is missing and -h is not set.
                   ; Be sure the user gets more context if the help-suffix
                   ; holds a list of possible commands.
                   (λ (e)
                     (printf "~a~n~a"
                             (exn-message e)
                             (if (and (regexp-match? #px"given 0 arguments" (exn-message e))
                                      suffix-is-index?
                                      (not help-requested?))
                                 help-suffix
                                 ""))
                     (halt 1))])
    (parse-command-line program argv
                        (if (null? flags)
                            null
                            `((once-each . ,flags)))
                        handle-arguments
                        arg-help-strings
                        (λ (help-str)
                          (printf "~a~n~a"
                                  help-str
                                  help-suffix)
                          (halt (if help-requested? 0 1))))))

; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
(module+ test
  (require racket/port
           racket/random
           racket/runtime-path
           rackunit
           (submod "file.rkt" test))

  (define-runtime-path here ".")

  (define (run-entry-point #:reload-config? [reload-config? #t] #:stdin [stdin (open-input-bytes #"")] args after)
    (define nout (current-output-port))
    (define stdout (open-output-bytes))
    (define stderr (open-output-bytes))
    (define-values (exit-code outbytes errbytes output)
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-input-port stdin])
        (define-values (code output) (entry-point #:reload-config? reload-config? args))
        (flush-output stdout)
        (flush-output stderr)
        (close-input-port stdin)
        (values code
                (open-input-bytes (get-output-bytes stdout #t))
                (open-input-bytes (get-output-bytes stderr #t))
                output)))

    (after exit-code
           outbytes
           errbytes
           output))

  (test-case "Configure zcpkg"
    (test-workspace "Respond to an incomplete command with help"
      (run-entry-point #("config")
                       (λ (exit-code stdout stderr output)
                         (check-eq? exit-code 1)
                         (check-true (andmap (λ (patt) (regexp-match? (regexp patt) stdout))
                                             '("given 0 arguments"
                                               "set"
                                               "get"
                                               "dump"))))))

    (test-workspace "Dump all (read)able configuration on request"
      (run-entry-point #("config" "dump")
                       (λ (exit-code stdout stderr output)
                         (check-eq? exit-code 0)
                         (check-equal? (dump-zcpkg-settings)
                                       (read stdout)))))

    (test-workspace "Return a (read)able config value"
      (define config-key (random-ref (in-hash-keys ZCPKG_SETTINGS)))
      (define config-key/str (symbol->string config-key))
      (run-entry-point (vector "config" "get" config-key/str)
                       (λ (exit-code stdout stderr output)
                         (check-eq? exit-code 0)
                         (check-equal? (get-zcpkg-setting-value config-key)
                                       (read stdout)))))

    (test-workspace "Save a (write)able config value"
      (run-entry-point (vector "config" "get" "ZCPKG_VERBOSE")
                       (λ _
                         ; This confirms that a new workspace has different results.
                         (check-false (ZCPKG_VERBOSE))
                         (check-false (file-exists? (get-zcpkg-settings-path)))))


      (run-entry-point (vector "config" "set" "ZCPKG_VERBOSE" "#t")
                       (λ (exit-code stdout stderr output)
                         (check-eq? exit-code 0)
                         (check-true (ZCPKG_VERBOSE))
                         (check-true (file-exists? (get-zcpkg-settings-path)))

                         ; Reload just for good measure.
                         (load-zcpkg-settings!)
                         (check-true (ZCPKG_VERBOSE))))))

  (test-workspace "Change a package's version"
                  (define package-name "foo")
                  (run-entry-point (vector "new" package-name) void)
                  (run-entry-point (vector "chver"
                                           (setting->short-flag ZCPKG_EDITION) "ed"
                                           (setting->short-flag ZCPKG_REVISION_NUMBER) "8"
                                           package-name
                                           "a" "b" "c")
                                   (λ (exit-code stdout stderr output)
                                     (check-eq? exit-code 0)
                                     (check-equal? (port->string stderr) "")
                                     (check-equal? (port->string stdout)
                                                   (format "~a:foo:draft:i:0:i:0 -> ~a:foo:ed:i:8:i:8\n"
                                                           (gethostname)
                                                           (gethostname)))

                                     (define info (read-zcpkg-info-from-directory package-name))
                                     (check-equal? (zcpkg-info-edition-name info) "ed")
                                     (check-equal? (zcpkg-info-revision-number info) 8)
                                     (check-equal? (zcpkg-info-revision-names info)
                                                   '("a" "b" "c")))))


  (test-workspace "Create a new package"
    (define package-name "foo")
    (run-entry-point (vector "new" "foo")
                     (λ (exit-code stdout stderr output)
                       (define pkg-dir (build-path (current-directory) package-name))
                       (check-eq? exit-code 0)
                       (check-pred directory-exists? pkg-dir)
                       (check-pred zcpkg-info? (read-zcpkg-info-from-directory pkg-dir)))))

  (test-workspace "Let zcpkg install itself"
                  (run-entry-point (vector "install" (setting->short-flag ZCPKG_CONSENT) (path->string here))
                                   (λ (exit-code stdout stderr output)
                                     (check-eq? exit-code 0))))

  (test-workspace "Install a local package with no dependencies"
    (define package-name "foo")
    (run-entry-point (vector "new" package-name) void)

    (define info (read-zcpkg-info-from-directory package-name))
    (define install-path (zcpkg-info->install-path info))

    ; Without explicit consent, the workspace filesystem does not change.
    ; The user is only alerted to what would happen if they gave consent.
    (run-entry-point (vector "install" package-name)
                     (λ (exit-code stdout stderr output)
                       ; Make sure the output mentions the package by name.

                       (check-true (regexp-match? (regexp package-name) stdout))
                       (check-false (directory-exists? install-path))
                       (check-eq? exit-code 0)))

    (run-entry-point (vector "install"
                             (setting->short-flag ZCPKG_CONSENT)
                             (setting->short-flag ZCPKG_LINK)
                             package-name)
                     (λ (exit-code stdout stderr output)
                       (check-equal?
                        (file-or-directory-identity install-path)
                        (file-or-directory-identity package-name))
                       (check-eq? exit-code 0)))

    ; Uninstallation has the same workflow. By default it does nothing but state its intentions.
    (run-entry-point (vector "uninstall" (zcpkg-query->string (zcpkg-info->zcpkg-query info)))
                     (λ (exit-code stdout stderr output)
                       (check-true (regexp-match? (regexp package-name) stdout))
                       (check-equal?
                        (file-or-directory-identity install-path)
                        (file-or-directory-identity package-name))
                       (check-eq? exit-code 0)))

    (run-entry-point (vector "uninstall"
                             (setting->short-flag ZCPKG_CONSENT)
                             (zcpkg-query->string (zcpkg-info->zcpkg-query info)))
                     (λ (exit-code stdout stderr output)
                       (check-false (directory-exists? install-path))
                       (check-true (directory-exists? package-name))
                       (check-eq? exit-code 0))))

  (test-workspace "Capture, Restore, and Diff workspace"
    (run-entry-point #("new" "foo") void)
    (run-entry-point `#("install" ,(setting->short-flag ZCPKG_CONSENT) "./foo") void)

    (call-with-output-file #:exists 'truncate/replace "other.rkt"
      (λ (o) (displayln "new!" o)))

    (run-entry-point #("capture" "\\.rkt$")
                     (λ (exit-code stdout stderr output)
                       (check-eq? exit-code 0)
                       (check-eqv? (length output) 1)
                       (check-pred $on-workspace-capture? (car output))
                       (call-with-output-file "cap.rkt"
                         (λ (to-capture-file) (copy-port stdout to-capture-file)))))

    (call-with-output-file #:exists 'truncate/replace "other.rkt"
      (λ (o) (displayln "change!" o)))

    (delete-file "foo/setup.rkt")

    (assume-settings ([ZCPKG_VERBOSE #t])
      (run-entry-point #("diff" "cap.rkt") #:reload-config? #f
                       (λ (exit-code stdout stderr output)
                         (check-eq? exit-code 0)
                         (check-equal? (set-subtract
                                        (set ($diff-different-file "other.rkt")
                                             ($diff-extra-file "cap.rkt")
                                             ($diff-missing-file "foo/setup.rkt")
                                             ($diff-same-file "foo/zcpkg.rkt"))
                                        (apply set output))
                                       (set))
                         (check-equal? (set-subtract
                                        (set "* other.rkt"
                                             "+ cap.rkt"
                                             "- foo/setup.rkt"
                                             "= foo/zcpkg.rkt")
                                        (apply set (string-split (port->string stdout) "\n")))
                                       (set))
                         (check-equal? (port->bytes stderr) #""))))

    (run-entry-point #("restore" "cap.rkt") #:reload-config? #f
                     (λ (exit-code stdout stderr output)
                       (check-equal? (port->bytes stderr) #"")
                       (check-true (> (length output) 1))
                       (define review (car output))

                       (test-pred "Emit $review-restoration-work in output"
                                  $review-restoration-work? review)

                       (test-equal? "Include capture file path in $restore-restoration-work"
                                    ($review-restoration-work-capture-file-path review)
                                    "cap.rkt")

                       (test-equal? "Plan to restore complete configuration"
                                    (apply set (hash-keys ZCPKG_SETTINGS))
                                    (apply set (map $restore-config-name (filter $restore-config? output))))

                       (test-case "Plan to restore captured packages"
                         (define package-work (filter $restore-package? output))
                         (check-eqv? (length package-work) 1)
                         (check-equal?
                          (car package-work)
                          ($restore-package
                           (zcpkg-query->string
                            (coerce-zcpkg-query
                             (read-zcpkg-info-from-directory "foo"))))))))))
