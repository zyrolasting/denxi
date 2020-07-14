#lang racket/base

(require racket/cmdline
         racket/path
         racket/pretty
         racket/list
         racket/match
         racket/path
         racket/sequence
         racket/set
         racket/vector
         (for-syntax racket/base)
         "server.rkt"
         "capture.rkt"
         "config.rkt"
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
         "zcpkg-info.rkt")


(module+ main
  (define maybe-exit (top-level-cli (current-command-line-arguments)))
  (exit (if (exact-nonnegative-integer? maybe-exit)
            maybe-exit
            0)))

(define (top-level-cli args)
  (run-command-line
   #:program "zcpkg"
   #:arg-help-strings '("action" "args")
   #:args args
   #:flags
   (list (option "-E" ZCPKG_SERVICE_ENDPOINTS)
         (switch "-c" ZCPKG_COLORIZE_OUTPUT)
         (switch "-v" ZCPKG_VERBOSE))

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
         ["restore" restore-command]
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
  capture    Create a restore point
  restore    Revert files according to a restore point
  diff       Compare files to restore point
  sandbox    Use untrusted package
  register   Register an account on a catalog
  serve      Serve installed packages
  download   Download packages and metadata
  upload     Upload packages and metadata

EOF
))


(define (install-command args)
  (run-command-line
   #:program "install"
   #:args args
   #:arg-help-strings '("package-source")
   #:flags
   (list (option "-E" ZCPKG_SERVICE_ENDPOINTS)
         (option "-m" ZCPKG_DOWNLOAD_MAX_REDIRECTS)
         (option "-r" ZCPKG_INSTALL_RELATIVE_PATH)
         (switch #f ZCPKG_TRUST_BAD_DIGEST)
         (switch #f ZCPKG_TRUST_BAD_SIGNATURE)
         (switch "-U" ZCPKG_TRUST_UNSIGNED)
         (switch "-i" ZCPKG_VERIFY_POST_INSTALL_INTEGRITY)
         (switch "-o" ZCPKG_INSTALL_ORPHAN)
         (switch "-y" ZCPKG_INSTALL_CONSENT)
         (switch "-x" ZCPKG_DOWNLOAD_IGNORE_CACHE))

   (λ (flags . package-sources)
     (define to-install (mutable-set))

     (define (find-dependencies source)
       (define variant (source->variant source))
       (define info (variant->zcpkg-info variant))
       (define deps (filter-missing-dependencies (zcpkg-info-dependencies info)))
       (set-add! to-install source)
       (for ([dep (in-list deps)])
         (set-add! to-install dep)
         (find-dependencies dep)))

     (for ([source (in-list package-sources)])
       (find-dependencies source))

     (if (ZCPKG_INSTALL_CONSENT)
         (process-jobs
          (for/list ([target (in-set to-install)])
            ($install-package target)))
         (begin
           ; For the users sake, show the potentially long installation list
           ; in lexographical order.
           (displayln (apply ~a*
                             (sort (set->list
                                    (set-subtract to-install
                                                  (apply set package-sources)))
                                   string<?)))
           (displayln "The above dependencies are needed to install")
           (displayln (apply ~a* package-sources))
           (displayln "Run with -y to consent to installation"))))))

(define (capture-command args)
  (run-command-line
   #:program "install"
   #:args args
   #:arg-help-strings '("package-source")
   (capture-workspace)))

(define (config-command args)
  (define (make-fail-thunk str)
    (λ ()
      (eprintf "There is no setting called ~a.~n" str)
      (exit 1)))

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
                                          ((hash-ref (dump-configuration)
                                                     (string->symbol key)
                                                     (make-fail-thunk key))))))]
       ["dump"
        (run-command-line #:args args
                          #:program "config-dump"
                          #:arg-help-strings '()
                          (pretty-write #:newline? #t
                                        (for/hash ([(k v) (dump-configuration)])
                                          (values k (v)))))]

       ["set"
        (run-command-line #:args args
                          #:program "config-set"
                          #:arg-help-strings '("key" "value")
                          (λ (flags key value)
                            (define dump (dump-configuration))
                            (define sym (string->symbol key))
                            (define param (hash-ref dump sym (make-fail-thunk key)))

                            (define to-write
                              (with-handlers ([exn:fail?
                                               (λ (e)
                                                 (eprintf
                                                  "Rejecting invalid value for ~a.~n"
                                                  sym)
                                                 (raise e))])
                                (param (read (open-input-string value)))
                                (param)))

                            (define target (ws/ "etc/zcpkg" key))
                            (make-directory* (path-only target))
                            (write-to-file #:exists 'truncate/replace
                                           (param) target)
                            (printf "Saved ~a~n" target)))]

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
  (run-command-line
   #:program "new"
   #:arg-help-strings '("package-name")
   #:args args
   (λ (flags name)
     (make-directory name)

     (define (write-info.rkt package-name)
       (displayln "#lang info\n")
       (writeln `(define provider-name "you"))
       (writeln `(define package-name ,package-name))
       (writeln `(define edition-name  "draft"))
       (writeln `(define revision-number 0))
       (displayln "(define revision-names '(\"initial\"))")
       (writeln `(define installer "installer.rkt"))
       (displayln "(define dependencies '())"))

     (define (write-installer.rkt package-name)
       (displayln "#lang racket/base\n")
       (displayln "; This module is meant for running in a sandboxed REPL."))

     (define (make-file file-name proc)
       (call-with-output-file (build-path name file-name)
         (λ (o) (parameterize ([current-output-port o])
                  (proc name)))))

     (make-file "info.rkt" write-info.rkt)
     (make-file "installer.rkt" write-installer.rkt))))


(define (serve-command args)
  (run-command-line
   #:program "serve"
   #:arg-help-strings null
   #:args args
   (λ (flags)
     (start-server)
     (printf "Service up at ~a~n^C to stop~n" (url->string (make-endpoint)))
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
        (displayln (ZCPKG_WORKSPACE))]
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
     (process-jobs
      (for/list ([ds (in-list urns)])
        ($uninstall-package ds))))))

(define (restore-command args)
  (run-command-line
   #:program "restore"
   #:arg-help-strings '("capture-file")
   #:args args
   (λ (flags capture-file)
     (restore-workspace capture-file))))

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

Do not leak confidential information in your payload.  If this
happens, you may upload replacements within a grace period set by the
server. After that, the artifact can only be removed by an
administrator.

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
                         (printf "~a~n~a" (exn-message e) help-suffix)
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


(define (make-boolean-flag flag-spec param)
  (list flag-spec
        (λ (flag) (param #t))
        (list " ")))

(define (make-value-flag flag-spec param)
  (list flag-spec
        (λ (flag v) (param (read (open-input-string v))))
        (list " " " ")))

(define-syntax (flag-names stx)
  (syntax-case stx ()
    [(_ short name)
     #'(let ([long (setting-id->cli-flag-string 'name)])
         (if short
             (list short long)
             (list long)))]))

(define-syntax-rule (switch short name)
  (make-boolean-flag (flag-names short name) name))

(define-syntax-rule (option short name)
  (make-value-flag (flag-names short name) name))
