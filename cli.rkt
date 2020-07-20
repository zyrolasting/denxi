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
         "server.rkt"
         "capture.rkt"
         "config.rkt"
         "contract.rkt"
         "dependency.rkt"
         "download.rkt"
         "file.rkt"
         "message.rkt"
         "setting.rkt"
         "source.rkt"
         "string.rkt"
         "team.rkt"
         "url.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt"
         "zcpkg-worker.rkt")


(module+ main
  (exit (entry-point)))

; Keep seperate for functional tests.
(define (entry-point [args (current-command-line-arguments)])
  (load-zcpkg-settings!)
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
    ZCPKG_VERBOSE)
   (λ (flags action . args)
     (define proc
       (match action
         ["install" install-command]
         ["uninstall" uninstall-command]
         ["new" new-command]
         ["serve" serve-command]
         ["config" config-command]
         ["show" show-command]
         ["capture" capture-command]
         ["download" download-command]
         ["upload" upload-command]
         [_ (printf "Unrecognized command: ~s. Run with -h for usage information.~n"
                    action)
            1]))
     (proc args))

#<<EOF
<action> is one of
  install    Install packages
  uninstall  Uninstall packages
  new        Create a new package
  show       Print helpful information
  config     Configure the package manager
  capture    Create a capture file
  sandbox    Start sandboxed REPL for package's setup module.
  register   Register an account on a catalog
  serve      Serve installed packages
  download   Download packages and metadata
  upload     Upload packages and metadata

EOF
))


(define (install-command args)
  (define (show-report output)
    (writeln output))


  (define (review-work package-sources sow)
    (define targets
      (map car (hash-values sow)))

    (define infos
      (sort targets
            #:key (λ (info) (dependency->string (zcpkg-info->dependency info)))
            string<?))

    (printf "~nSources:~n~a~n~n"
            (string-join (map (λ (s) (~a "  " s)) package-sources) "\n"))

    (define (get-cell-printer strs)
      (define min-width (apply max (map string-length strs)))
      (λ args (apply ~a #:min-width min-width args)))


    (define print-provider-name (get-cell-printer (map zcpkg-info-provider-name infos)))
    (define print-package-name  (get-cell-printer (map zcpkg-info-package-name infos)))
    (define print-edition-name  (get-cell-printer (map zcpkg-info-edition-name infos)))
    (define print-revision-num  (get-cell-printer (map (compose ~a zcpkg-info-revision-number) infos)))

    (define row-fmt "~a\t~a\t~a\t~a")
    (printf (~a (format row-fmt
                        (print-package-name "Package")
                        (print-provider-name "Provider")
                        (print-edition-name "Edition")
                        (print-revision-num "Revision"))
                "~n~a~n~n")
            (string-join
             (for/list ([info (in-list infos)])
               (format row-fmt
                       (print-package-name (zcpkg-info-package-name info))
                       (print-provider-name (zcpkg-info-provider-name info))
                       (print-edition-name (zcpkg-info-edition-name info))
                       (print-revision-num (zcpkg-info-revision-number info))))
             "\n"))

    (displayln "To consent to these changes, run again with -y"))

  (define (do-work sow)
    (define controller (zcpkg-start-team!))
    (dynamic-wind
      void
      (λ ()
        (controller ($start (workspace-directory)))

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
    ZCPKG_INSTALL_CONSENT
    ZCPKG_DOWNLOAD_IGNORE_CACHE)

   (λ (flags . package-sources)
     (define sow (find-scope-of-work package-sources))

     (if (ZCPKG_INSTALL_CONSENT)
         (do-work sow)
         (review-work package-sources sow)))))

(define (capture-command args)
  (run-command-line
   #:program "install"
   #:args args
   #:arg-help-strings null
   (λ (flags) (capture-workspace))))

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

       [_ (printf "Unrecognized command: ~s. Run with -h for usage information.~n"
                  action)
          1]))

   #<<EOF
<action> is one of
  set   Set a new value for a setting
  get   Get the current value of a setting
  dump  Write a hash table of all settings

EOF
))



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
                 (<< '#:provider (gethostname))
                 (<< '#:package name)
                 (<< '#:edition "draft")
                 (<< '#:revision-number 0)
                 (<< '#:revision-names null)
                 (<< '#:setup-module "setup.rkt")
                 (<< '#:dependencies null)))

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
   #:args args
   (λ (flags . urns)
     (define controller (zcpkg-start-team!))
     (define backlog (controller (map $uninstall-package urns)))
     (controller backlog))))

(define (download-command args)
  (run-command-line
   #:program "download"
   #:arg-help-strings '("kind" "target")
   #:args args
   (λ (flags kind u)
     (void))
   #<<EOF
where <kind> is one of
  info     Download package metadata. <target> is a URL
  package  A package. <target> is a downloaded info file.
EOF
))

(define (upload-command args)
  (run-command-line
   #:program "upload"
   #:arg-help-strings '("info-file" "package-directory")
   #:args args
   (λ (flags kind u)
     (void))
   #<<EOF

=====================================================================
                       /!\   WARNING   /!\
=====================================================================

Be sure that you did not leave any confidential information in your
package.

Once you upload your package, you may upload replacements within a
grace period set by the server. After that, it can only be removed by
an administrator.

EOF
))



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
                                  help-suffix)))))

; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
(module+ test
  (require racket/port
           racket/random
           rackunit
           (submod "file.rkt" test))

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

  (test-workspace "Create a new package"
    (define package-name "foo")
    (run-entry-point (vector "new" "foo")
                     (λ (exit-code stdout stderr)
                       (define pkg-dir (build-path (current-directory) package-name))
                       (check-eq? exit-code 0)
                       (check-pred directory-exists? pkg-dir)
                       (check-pred zcpkg-info? (read-zcpkg-info-from-directory pkg-dir)))))

  (test-workspace "Install a local package with no dependencies"
    (define package-name "foo")
    (run-entry-point (vector "new" "foo") void)
    (run-entry-point (vector "install" "./foo")
                     (λ (exit-code stdout stderr)
                       (copy-port stdout (current-output-port))
                       (check-eq? exit-code 0)))))
