#lang racket/base

(require racket/cmdline
         racket/path
         racket/pretty
         racket/list
         racket/match
         racket/os
         racket/path
         racket/port
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
  (exit (entry-point)))

; Keep seperate for functional tests.
(define (entry-point #:reload-config? [reload-config? #t] [args (current-command-line-arguments)])
  (when reload-config?
    (reset-zcpkg-setting-overrides!)
    (load-zcpkg-settings!))
  (define maybe-exit
    (with-handlers ([exact-nonnegative-integer? values])
      (top-level-cli args)))
  (if (exact-nonnegative-integer? maybe-exit)
      maybe-exit
      0))

(define (top-level-cli args)
  (run-command-line
   #:program "zcpkg"
   #:arg-help-strings '("action" "args")
   #:args args
   #:flags
   (settings->flag-specs
    ZCPKG_SERVICE_ENDPOINTS
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
  (define (show-report output)
    (for ([m (in-list output)])
      (displayln (format-zcpkg-message m))))

  (define (review-work package-sources sow)
    (write-output ($review-installation-work sow package-sources)))

  (define (do-work sow)
    (define controller (zcpkg-start-team!))
    (dynamic-wind
      void
      (λ ()
        (controller ($start (workspace-directory)
                            ((current-zcpkg-config) 'dump)))

        (define tasks
          (for/list ([(url-or-path infos) (in-hash sow)])
            ($install-package infos url-or-path)))

        (show-report (controller tasks)))
      (λ () (controller #f))))

  (run-command-line
   #:program "install"
   #:args args
   #:arg-help-strings '("package-source")
   #:flags
   (settings->flag-specs
    ZCPKG_SERVICE_ENDPOINTS
    ZCPKG_DOWNLOAD_MAX_REDIRECTS
    ZCPKG_INSTALL_RELATIVE_PATH
    ZCPKG_TRUST_BAD_DIGEST
    ZCPKG_TRUST_BAD_SIGNATURE
    ZCPKG_TRUST_UNSIGNED
    ZCPKG_INSTALL_ORPHAN
    ZCPKG_CONSENT
    ZCPKG_DOWNLOAD_IGNORE_CACHE)

   (λ (flags . package-sources)
     (define sow (find-scope-of-work package-sources))

     (when (null? package-sources)
       (raise (exn:fail:user "No package sources specified." (current-continuation-marks))))

     (if (ZCPKG_CONSENT)
         (do-work sow)
         (review-work package-sources sow)))))

(define (capture-command args)
  (run-command-line
   #:program "capture"
   #:args args
   #:arg-help-strings '("pregexp-strings")
   (λ (flags . file-patterns)
     (write-capture (capture-workspace (pattern-map file-patterns))))))


(define (restore-command args)
  (define (run-commands cmds)
    (for ([cmd (in-list cmds)])
      (entry-point #:reload-config? #f cmd)))

  (define (show-commands cmds)
    (for ([cmd (in-list cmds)])
      (displayln (string-join (vector->list cmd) " "))))

  (define (do-work capture-file configure-commands install-commands)
    (run-commands configure-commands)
    (reset-zcpkg-setting-overrides!)
    (load-zcpkg-settings!)
    (run-commands install-commands)
    (run-commands (list (vector "diff" capture-file))))

  (define (show-work capture-file configure-commands install-commands)
    (printf #<<EOS
# These commands install packages under a specific configuration.
# The configuration controls whether zcpkg trusts unsigned
# packages or bad digests, so please review this carefully.
#
# You can adapt this output to a script on your operating system, or you can
# run the restore command again with ~a to execute these instructions.


EOS
(setting->short-flag ZCPKG_CONSENT))
    (show-commands
      (map (λ (cmd)
       (vector "zcpkg" "config" "set"
               (vector-ref cmd 2)
               (~s (vector-ref cmd 3))))
           configure-commands))

    (show-commands
      (map (λ (cmd)
             (vector "zcpkg" "install" (setting->short-flag ZCPKG_CONSENT)
                     (~s (vector-ref cmd 2))))
           install-commands))

    (show-commands (list (vector "zcpkg" "diff" (~s capture-file)))))

  (run-command-line
   #:program "restore"
   #:args args
   #:flags
   (settings->flag-specs ZCPKG_CONSENT)
   #:arg-help-strings '("capture-file")
   (λ (flags capture-file)
     (define lookup (load-config (string->path capture-file)))
     (define config (lookup 'config))
     (define packages (lookup 'packages))

     (define config-commands
       (for/list ([(k v) (in-hash config)])
         (vector "config" "set" (~a k) v)))

     (define install-commands
       (for/list ([pkg (in-list packages)])
         (vector "install" (setting->short-flag ZCPKG_CONSENT) pkg)))

     (if (ZCPKG_CONSENT)
         (do-work capture-file config-commands install-commands)
         (show-work capture-file config-commands install-commands)))))

(define (diff-command args)
  (define (print-capture-diff ours theirs)
    (define all-paths (apply set (append (hash-keys ours) (hash-keys theirs))))
    (for ([path (in-set all-paths)])
      (define sym (compare-path path ours theirs))
      (unless (eq? sym '=)
        (printf "~a ~a~n" sym path))))

  (run-command-line
   #:program "capture"
   #:args args
   #:arg-help-strings '("capture-module")
   (λ (flags capture-path . file-patterns)
     (define lookup (load-config (string->path capture-path)))
     (print-capture-diff (hash-ref (capture-workspace (pattern-map file-patterns)) 'digests)
                         (lookup 'digests)))))


(define (pattern-map patts)
  (map pregexp
       (if (null? patts)
           '("\\.(rkt|scrbl|ss|dep|zo)$")
           patts)))


(define (link-command args)
  (run-command-line
   #:program "link"
   #:args args
   #:arg-help-strings '("nss" "link-path")
   (λ (flags source link-path)
     (define seq (search-zcpkg-infos source (in-installed-info)))
     (if (= (sequence-length seq) 0)
         (begin (printf "Cannot find a package using ~s.~n" source)
                1)
         (begin
           (make-file-or-directory-link
            (zcpkg-info->install-path
             (sequence-ref seq 0))
            link-path))))))


(define (config-command args)
  (define (make-fail-thunk str)
    (λ ()
      (eprintf "There is no setting called ~a.~n" str)
      (exit 1)))

  (define controller (current-zcpkg-config))

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
                                          (controller
                                           'get-value
                                           (string->symbol key)
                                           (make-fail-thunk key)))))]
       ["dump"
        (run-command-line #:args args
                          #:program "config-dump"
                          #:arg-help-strings '()
                          (λ (flags)
                            (pretty-write #:newline? #t
                                          (controller 'dump))))]

       ["set"
        (run-command-line #:args args
                          #:program "config-set"
                          #:arg-help-strings '("key" "value")
                          (λ (flags key value)
                            (define sym (string->symbol key))
                            (define picked-setting
                              (controller 'get-setting sym (make-fail-thunk key)))

                            (define to-write
                              (with-handlers ([exn:fail?
                                               (λ (e)
                                                 (eprintf
                                                  "Rejecting invalid value for ~a.~n"
                                                  sym)
                                                 (raise e))])
                                (picked-setting (read (open-input-string value)))
                                (picked-setting)))

                            (controller 'save!)
                            (printf "Saved ~a~n" (controller 'get-path))))]

       [_ (write-output ($unrecognized-command action))
          1]))

   #<<EOF
<action> is one of
  set   Set a new value for a setting
  get   Get the current value of a setting
  dump  Write a hash table of all settings

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
                          (printf "Could not read ~a. Double check that ~s points to a package directory.~n"
                                  CONVENTIONAL_PACKAGE_INFO_FILE_NAME
                                  package-path-string)
                          (raise 1))])
         (read-zcpkg-info-from-directory package-path-string)))

     (define errors (validate-zcpkg-info info))
     (unless (null? errors)
       (printf (~a "Cannot change version.~n"
                   "There are errors in the original info:~n~a~n")
               (string-join errors "\n"))
       (raise 1))

     (define new-info
       (struct-copy zcpkg-info info
                    [edition-name (if (ZCPKG_EDITION)
                                      (~a (ZCPKG_EDITION))
                                      (zcpkg-info-edition-name info))]
                    [revision-number (if (ZCPKG_REVISION_NUMBER)
                                         (ZCPKG_REVISION_NUMBER)
                                         (add1 (zcpkg-info-revision-number info)))]
                    [revision-names revision-names]))

     (define new-errors (validate-zcpkg-info new-info))
     (unless (null? errors)
       (printf (~a "Cannot change version.~n"
                   "There are errors in the new info:~n~a~n")
               (string-join new-errors "\n"))
       (raise 1))

     (call-with-output-file
       #:exists 'truncate/replace
       (build-path package-path-string
                   CONVENTIONAL_PACKAGE_INFO_FILE_NAME)
       (λ (o) (write-zcpkg-info new-info o)))

     (printf "~a -> ~a~n"
             (format-zcpkg-info info)
             (format-zcpkg-info new-info)))))


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
                          (printf "Could not read ~a. Double check that ~s points to a package directory.~n"
                                  CONVENTIONAL_PACKAGE_INFO_FILE_NAME
                                  package-path-string)
                          (raise 1))])
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
                    [else (raise (exn:fail:user (format (~a "Cannot bundle a package with a dependency on ~s.~n"
                                                            "Use a path or a package query.~n")
                                                        dep)
                                                (current-continuation-marks)))]))
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
       (build-path archive-directory "zcpkg.rktd"))

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
           (displayln "The patterns specified did not match any files.")
           (displayln "Stopping.")
           (raise 1))

         (pack archive-file archive-files)))

     (define-values (exit-code/digest digest)
       (make-digest archive))

     (unless (= exit-code/digest 0)
       (printf "OpenSSL exited with code ~s when creating a digest~n" exit-code/digest)
       (raise 1))

     (define-values (exit-code/signature signature)
       (if (ZCPKG_PRIVATE_KEY_PATH)
           (make-signature digest (ZCPKG_PRIVATE_KEY_PATH))
           (values 0 #f)))

     (unless (= exit-code/signature 0)
       (printf "OpenSSL exited with code ~s when creating a signature~n" exit-code/signature)
       (raise 1))

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
       (printf "Wrote ~a~n" created)))))



(define (new-command args)
  (define (make-file dir file-name proc)
    (call-with-output-file (build-path dir file-name)
      (λ (o) (parameterize ([current-output-port o])
               (proc)))))


  (define (make-package name)
    (make-directory name)

    (make-file name CONVENTIONAL_PACKAGE_INFO_FILE_NAME
               (λ ()
                 (define (<< k v)
                   (printf "~s ~s~n" k v))
                 (write-config
                  (hasheq 'provider (gethostname)
                          'package name
                          'edition "draft"
                          'revision-number 0
                          'revision-names '()
                          'setup-module "setup.rkt"
                          'dependencies '())
                  '(provider
                    package
                    edition
                    revision-number
                    revision-names
                    setup-module
                    dependencies)
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
     (call/cc
      (λ (k)
        (define targets (cons name names))
        (define existing
          (filter-map (λ (t)
                        (and (or (file-exists? t)
                                 (directory-exists? t)
                                 (link-exists? t))
                             t))
                      targets))

        (unless (null? existing)
          (printf "Cannot make new package(s). The following files or directories already exist:~n~a~n"
                  (apply ~a* existing))
          (k 1))

        (for ([t (in-list targets)])
          (make-package t)))))))


(define (serve-command args)
  (run-command-line
   #:program "serve"
   #:arg-help-strings null
   #:args args
   (λ (flags)
     (start-server)
     (printf "Service up. ^C to stop~n")
     (with-handlers ([exn:break? (λ (e) (displayln "bye"))])
       (sync/enable-break never-evt)))))


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
        (displayln (workspace-directory))]
       [_ (printf "Unrecognized argument for show command: ~s~n"
                  what)
          (printf "Run with -h for valid arguments.")
          (exit 1)]))
   #<<EOF
where <what> is one of
  workspace  The current target workspace directory

EOF
))

(define (uninstall-command args)
  (run-command-line
   #:program "uninstall"
   #:arg-help-strings '("urns")
   #:flags
   (settings->flag-specs
    ZCPKG_LEAVE_ORPHANS
    ZCPKG_CONSENT)
   #:args args
   (λ (flags . urns)
     (define to-uninstall (mutable-set))
     (define will-be-orphaned (mutable-set))

     ; Determine impact to the system
     (for ([urn (in-list urns)])
       (define info (find-exactly-one-info urn))
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
           (printf "Deleting ~a~n" install-path)
           (delete-directory/files/empty-parents install-path))
         (write-output
          ($review-uninstallation-work
           (sequence->list
            (sequence-append (in-mutable-set to-uninstall)
                             (in-mutable-set will-be-orphaned)))))))))



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
                     (if (and (regexp-match? #px"given 0 arguments" (exn-message e))
                              suffix-is-index?
                              (not help-requested?))
                         (begin (printf "~a~n~a" (exn-message e) help-suffix)
                                (raise 1))
                         (raise e)))])
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
                          (raise (if help-requested? 0 1))))))

; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
(module+ test
  (require racket/port
           racket/random
           racket/runtime-path
           rackunit
           (submod "file.rkt" test))

  (define-runtime-path here ".")

  (define (run-entry-point #:stdin [stdin (open-input-bytes #"")] args after)
    (define nout (current-output-port))
    (define stdout (open-output-bytes))
    (define stderr (open-output-bytes))
    (define-values (exit-code outbytes errbytes)
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-input-port stdin])
        (define code (entry-point args))
        (flush-output stdout)
        (flush-output stderr)
        (close-input-port stdin)
        (values code
                (open-input-bytes (get-output-bytes stdout #t))
                (open-input-bytes (get-output-bytes stderr #t)))))

    (after exit-code
           outbytes
           errbytes))

  (test-case "Configure zcpkg"
    (test-workspace "Respond to an incomplete command with help"
      (run-entry-point #("config")
                       (λ (exit-code stdout stderr)
                         (check-eq? exit-code 1)
                         (check-true (andmap (λ (patt) (regexp-match? (regexp patt) stdout))
                                             '("given 0 arguments"
                                               "set"
                                               "get"
                                               "dump"))))))

    (test-workspace "Dump all (read)able configuration on request"
      (run-entry-point #("config" "dump")
                       (λ (exit-code stdout stderr)
                         (check-eq? exit-code 0)
                         (check-equal? ((current-zcpkg-config) 'dump)
                                       (read stdout)))))

    (test-workspace "Return a (read)able config value"
      (define config-key (random-ref (in-hash-keys ZCPKG_SETTINGS)))
      (define config-key/str (symbol->string config-key))
      (run-entry-point (vector "config" "get" config-key/str)
                       (λ (exit-code stdout stderr)
                         (check-eq? exit-code 0)
                         (check-equal? ((current-zcpkg-config) 'get-value config-key)
                                       (read stdout)))))

    (test-workspace "Save a (write)able config value"
      (run-entry-point (vector "config" "get" "ZCPKG_VERBOSE")
                       (λ _
                         ; This confirms that a new workspace has different results.
                         (check-false (ZCPKG_VERBOSE))
                         (check-false (file-exists? (get-zcpkg-settings-path)))))


      (run-entry-point (vector "config" "set" "ZCPKG_VERBOSE" "#t")
                       (λ (exit-code stdout stderr)
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
                                   (λ (exit-code stdout stderr)
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
                     (λ (exit-code stdout stderr)
                       (define pkg-dir (build-path (current-directory) package-name))
                       (check-eq? exit-code 0)
                       (check-pred directory-exists? pkg-dir)
                       (check-pred zcpkg-info? (read-zcpkg-info-from-directory pkg-dir)))))

  (test-workspace "Let zcpkg install itself"
                  (run-entry-point (vector "install" (setting->short-flag ZCPKG_CONSENT) (path->string here))
                                   (λ (exit-code stdout stderr)
                                     (check-eq? exit-code 0))))

  (test-workspace "Install a local package with no dependencies"
    (define package-name "foo")
    (run-entry-point (vector "new" package-name) void)

    (define info (read-zcpkg-info-from-directory package-name))
    (define install-path (zcpkg-info->install-path info))

    ; Without explicit consent, the workspace filesystem does not change.
    ; The user is only alerted to what would happen if they gave consent.
    (run-entry-point (vector "install" package-name)
                     (λ (exit-code stdout stderr)
                       ; Make sure the output mentions the package by name.
                       (check-true (regexp-match? (regexp package-name) stdout))
                       (check-false (directory-exists? install-path))
                       (check-eq? exit-code 0)))

    (run-entry-point (vector "install" (setting->short-flag ZCPKG_CONSENT) package-name)
                     (λ (exit-code stdout stderr)
                       (check-equal?
                        (file-or-directory-identity install-path)
                        (file-or-directory-identity package-name))
                       (check-eq? exit-code 0)))

    ; Uninstallation has the same workflow. By default it does nothing but state its intentions.
    (run-entry-point (vector "uninstall" (zcpkg-query->string (zcpkg-info->zcpkg-query info)))
                     (λ (exit-code stdout stderr)
                       (check-true (regexp-match? (regexp package-name) stdout))
                       (check-equal?
                        (file-or-directory-identity install-path)
                        (file-or-directory-identity package-name))
                       (check-eq? exit-code 0)))

    (run-entry-point (vector "uninstall"
                             (setting->short-flag ZCPKG_CONSENT)
                             (zcpkg-query->string (zcpkg-info->zcpkg-query info)))
                     (λ (exit-code stdout stderr)
                       (check-false (directory-exists? install-path))
                       (check-true (directory-exists? package-name))
                       (check-eq? exit-code 0)))))
