#lang racket/base

(require racket/class
         racket/function
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
         "cmdline.rkt"
         "config.rkt"
         "contract.rkt"
         "derivation.rkt"
         "file.rkt"
         "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "monad.rkt"
         "package.rkt"
         "package-info.rkt"
         "printer.rkt"
         "query.rkt"
         "rc.rkt"
         "sentry.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "team.rkt"
         "url.rkt"
         "openssl.rkt"
         "worker.rkt"
         "workspace.rkt")


(module+ main
  (exit (entry-point (current-command-line-arguments))))


; Keep seperate for functional tests.
(define (entry-point args)
  (do (if (show-workspace-envvar-error?)
          (attach-message args ($invalid-workspace-envvar))
          (omit-message args))
      top-level-cli))


(define (top-level-cli args)
  (run-command-line
   #:program "xiden"
   #:arg-help-strings '("action" "args")
   #:args args
   #:flags
   (settings->flag-specs
    XIDEN_FASL_OUTPUT
    XIDEN_READER_FRIENDLY_OUTPUT
    XIDEN_VERBOSE)
   (λ (flags action . args)
     (with-rc flags
       (define proc
         (match action
           ["install" install-command]
           ["uninstall" uninstall-command]
           ["show" show-command]
           ["link" link-command]
           ["config" config-command]
           ["sandbox" sandbox-command]
           [_ (const (attach-message 1 ($unrecognized-command action)))]))
       (proc args)))

   #<<EOF
<action> is one of
  install    Install packages
  uninstall  Uninstall packages
  show       Print helpful information
  link       Create symlink to package file
  config     Manage configuration
  sandbox    Start sandboxed REPL in package

EOF
   ))


(define (install-command args)
  (run-command-line
   #:program "install"
   #:args args
   #:arg-help-strings '("pkgdef-source")
   #:flags
   (settings->flag-specs
    XIDEN_SERVICE_ENDPOINTS
    XIDEN_DOWNLOAD_MAX_REDIRECTS
    XIDEN_TRUST_BAD_DIGEST
    XIDEN_TRUST_BAD_SIGNATURE
    XIDEN_TRUST_UNSIGNED
    XIDEN_CONSENT
    XIDEN_LINK
    XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS
    XIDEN_ALLOW_UNSUPPORTED_RACKET)
   (λ (flags . pkgdef-sources)
     ; Source -> package definition
     ; Package definition -> derivation
     ; Run derivation
     ; Find in trash if needed
     (with-rc flags
       (do source  <- (return (in-list pkgdef-sources))
           (return source))))))


(define (uninstall-command args)
  (run-command-line
   #:program "uninstall"
   #:arg-help-strings '("query")
   #:flags
   (settings->flag-specs XIDEN_CONSENT)
   #:args args
   ; Proceed as if you were installing, until you
   ; find package paths. Check database for
   ; dependents referencing the paths.
   ; Move to trash
   (λ (flags . sources)
     (void))))


(define (link-command args)
  (run-command-line
   #:program "link"
   #:args args
   #:arg-help-strings '("link-path" "query" "rel-path")
   (λ (flags link-path query rel-path)
     (do mid <- (find-latest-package-id (coerce-xiden-query query))
         (return
          (if mid
              (make-link/clobber (build-path (get-derivation-directory mid)
                                             rel-path)
                                 link-path)
              (attach-message 1 ($show-string "No package found"))))))))


(define (config-command args)
  (define (get-setting name)
    (define maybe-selected-setting (setting-ref name))
    (if maybe-selected-setting
        (omit-message maybe-selected-setting)
        (attach-message #f ($setting-not-found name))))

  (run-command-line
   #:program "config"
   #:args args
   #:arg-help-strings '("action" "args")

   (λ (flags action . args)
     (match action
       ["get"
        (run-command-line
         #:args args
         #:program "config-get"
         #:arg-help-strings '("key")
         (λ (flags name)
           (define maybe-selected-setting (setting-ref name))
           (if maybe-selected-setting
               (attach-message 0 ($show-datum (maybe-selected-setting)))
               (attach-message 1 ($setting-not-found name)))))]


       ["dump"
        (run-command-line
         #:args args
         #:program "config-dump"
         #:arg-help-strings '()
         (λ (flags)
           (attach-message 0 ($show-datum (dump-xiden-settings)))))]


       ["set"
        (run-command-line
         #:args args
         #:program "config-set"
         #:arg-help-strings '("key" "value")
         (λ (flags name value-string)
           (define maybe-selected-setting (setting-ref name))
           (if maybe-selected-setting
               (with-handlers ([exn:fail?
                                (λ (e)
                                  (attach-message 1
                                                  ($setting-value-rejected
                                                   (setting-id maybe-selected-setting)
                                                   value-string
                                                   (if (exn:fail:contract? e)
                                                       (cadr (regexp-match #px"expected:\\s+([^\n]+)"
                                                                           (exn-message e)))
                                                       (exn-message e)))))])
                 (attach-message 0
                                 (maybe-selected-setting (read (open-input-string value-string))
                                                         save-xiden-settings!)))
               (attach-message 1 ($setting-not-found name)))))]

       [_
        (attach-message 1 ($unrecognized-command action))]))

   #<<EOF
<action> is one of
  set   Set a new value for a setting
  get   Get the current value of a setting
  dump  Write a hash table of all settings

EOF
   ))




(define (sandbox-command args)
  (run-command-line
   #:program "sandbox"
   #:arg-help-strings '("query")
   #:flags
   (settings->flag-specs
    XIDEN_SANDBOX_MEMORY_LIMIT_MB
    XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB
    XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
   #:args args
   (λ (flags query)
     (define input-program "TODO")
     (define build-directory "TODO")
     (parameterize ([current-eval (make-build-sandbox input-program build-directory)])
       (read-eval-print-loop)))))


(define (show-command args)
  (run-command-line
   #:args args
   #:program "show"
   #:arg-help-strings '("what")
   (λ (flags what)
     (match what
       ["workspace"
        (attach-message 0 ($show-string (path->string ((workspace-directory)))))]
       ["installed"
        (define in-installed '(TODO database query))
        (attach-message 0 (for/list ([path in-installed])
                 ($show-string path)))]
       [_
        (attach-message 1 ($unrecognized-command what))]))
   #<<EOF
where <what> is one of
  workspace  The current target workspace directory
  installed  A list of installed packages

EOF
   ))


#;(define (format-package-info info)
  (package-info-package-name info))

#;(define (format-setting-flag-example s)
  (format "~a/~a"
          (setting-short-flag s)
          (setting-long-flag s)))

#;(define (format-xiden-message m)
  (match m
    [($test-print v)
     (format "Testing: ~a" v)]

    [($output v)
     (format-xiden-message v)]

    [($fail v)
     (cond [(exn? v) (exn->string v)]
           [(string? v) v]
           [else (~s v)])]

    [($show-string v) v]

    [($show-datum v)
     (pretty-format #:mode 'write v)]

    [($module-compiled module-path)
     (format "Compiled: ~a" module-path)]

    [($compilation-error module-path message)
     (format "Bytecode compilation error in: ~a~n~a"
             module-path
             message m)]

    [($fetch-integrity-mismatch input source)
     (format (~a "~a failed its integrity check.~n"
                 "While unsafe, you can force installation using ~a.")
             (format-package-info source)
             (setting-long-flag XIDEN_TRUST_BAD_DIGEST))]

    [($mod-load-failure path error-string)
     (format (~a "Could not load plugin module ~a. Using default implementations.~n"
                 "Load error: ~a")
             path
             error-string)]

    [($fetch-signature-mismatch input source)
     (format (~a "~s's signature does not match any trusted public key.~n"
                 "While unsafe, you can trust bad signatures using ~a.")
             source
             (setting-long-flag XIDEN_TRUST_BAD_SIGNATURE))]

    [($fetch-signature-missing (fetch-info name _ _ _) source)
     (format (~a "~a does not have a signature. If you are testing a package, this is expected.~n"
                 "If you got the package from the Internet, then exercise caution!~n"
                 "To trust unsigned packages, use ~a.")
             name
             (setting-long-flag XIDEN_TRUST_UNSIGNED))]

    [($unverified-host url)
     (format (~a "~a does not have a valid certificate.~n"
                 "Connections to this server are not secure.~n"
                 "To trust servers without valid certificates, use ~a.")
             url
             (setting-long-flag XIDEN_TRUST_UNVERIFIED_HOST))]

    [($package-installed info)
     (format "Installed package ~a"
             (format-package-info info))]

    [($unrecognized-command m)
     (format "Unrecognized command: ~s. Run with -h for usage information.~n"
             ($unrecognized-command-command m))]

    [($consent-note)
     (format "To consent to these changes, run again with ~a"
             (setting-short-flag XIDEN_CONSENT))]

    [($source-unfetched user-string)
     (format "Cannot find content for ~s" user-string)]

    [($source-fetched user-string)
     (format "Fetched ~s" user-string)]

    [($setting-not-found name)
     (format "There is no setting called ~s.~n" name)]

    [($init-localstate path)
     (format "Initalizing local state at ~a" path)]

    [($setting-value-rejected name value expected)
     (format "Invalid value for ~a: ~a~n  expected: ~a~n  (Note: (void) only applies for `xiden config repl` use)"
             name
             value
             expected)]

    [($invalid-workspace-envvar)
     (format "Ignoring envvar value for XIDEN_WORKSPACE: ~a~n  falling back to ~a"
             (getenv "XIDEN_WORKSPACE")
             (workspace-directory))]

    [($undeclared-racket-version info)
     (join-lines
      (list (format "~a does not declare a supported Racket version."
                    (format-package-info info))
            (format "To install this package anyway, run again with ~a"
                    (setting-short-flag XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))))]

    [($unsupported-racket-version info)
     (join-lines
      (list (format "~a claims that it does not support this version of Racket (~a)."
                    (format-package-info info)
                    (version))
            (format "Supported versions (ranges are inclusive):~n~a~n"
                    (join-lines
                     (map (λ (variant)
                            (format "  ~a"
                                    (if (pair? variant)
                                        (format "~a - ~a"
                                                (or (car variant)
                                                    PRESUMED_MINIMUM_RACKET_VERSION)
                                                (or (cdr variant)
                                                    PRESUMED_MAXIMUM_RACKET_VERSION))
                                        variant))
                            (package-info-racket-versions info)))))
            (format "To install this package anyway, run again with ~a"
                    (setting-long-flag XIDEN_ALLOW_UNSUPPORTED_RACKET))))]

    [_ (error 'format-xiden-message "Unknown message type: ~s" m)]))


; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
#;(module+ test
  (require racket/port
           racket/random
           rackunit
           (submod "file.rkt" test))

  (test-case "Configure xiden"
    (test-workspace "Respond to an incomplete command with help"
                    (define out (entry-point '("config")))
                    (define help-message (findf $show-string? ($with-messages-accumulated out)))
                    (check-eq? ($with-messages-intermediate out) 1)
                    (check-true
                     (andmap (λ (patt) (regexp-match? (regexp patt) ($show-string-message help-message)))
                             '("given 0 arguments"
                               "set"
                               "get"
                               "dump"))))

    (test-workspace "Dump all (read)able configuration on request"
                    (define out (entry-point '("config" "dump")))
                    (check-eq? ($with-messages-intermediate out) 0)
                    (check-equal? ($with-messages-accumulated out)
                                  (list ($show-datum (dump-xiden-settings)))))

    (test-workspace "Return a (read)able config value"
                    (define config-key (random-ref (in-hash-keys XIDEN_SETTINGS)))
                    (define config-key/str (symbol->string config-key))
                    (define out (entry-point `("config" "get" ,config-key/str)))
                    (check-eq? ($with-messages-intermediate out) 0)
                    (check-equal? (car ($with-messages-accumulated out))
                                  ($show-datum ((hash-ref XIDEN_SETTINGS config-key)))))

    (test-workspace "Save a (write)able config value"
                    ; This confirms that a new workspace has different results.
                    (check-false (XIDEN_VERBOSE))
                    (check-false (file-exists? (get-xiden-settings-path)))

                    (define get/output (entry-point '("config" "get" "XIDEN_VERBOSE")))
                    (define set/output (entry-point '("config" "set" "XIDEN_VERBOSE" "#t")))

                    (check-pred file-exists? (get-xiden-settings-path))
                    (check-true ((load-xiden-rcfile) 'XIDEN_VERBOSE)))))
