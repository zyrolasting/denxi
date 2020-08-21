#lang racket/base

(require racket/class
         racket/cmdline
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
         "config.rkt"
         "contract.rkt"
         "file.rkt"
         "format.rkt"
         "input-info.rkt"
         "input-forms-lang.rkt"
         "integrity.rkt"
         "message.rkt"
         "output.rkt"
         "package.rkt"
         "package-info.rkt"
         "printer.rkt"
         "query.rkt"
         "rc.rkt"
         "resolve.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "team.rkt"
         "url.rkt"
         "openssl.rkt"
         "worker.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")


(module+ main
  (match-define ($with-output stop-value _ accumulated)
    (entry-point (current-command-line-arguments)))
  (sequence-for-each write-output accumulated)
  (exit stop-value))

; Define a transition from accumulated command line flags to a new parameterization
; in terms of those flags. Capture any failure to do so as main program output.
(define-syntax-rule (with-flags flags body ...)
  (with-handlers ([exn:fail? (λ (e) (output-success ($fail (exn-message e))))])
    (call-with-applied-settings flags (λ () body ...))))


; Keep seperate for functional tests.
(define (entry-point args)
  (output-fold args
               (λ (args)
                 (output-return args
                                (if (show-workspace-envvar-error?)
                                    ($invalid-workspace-envvar)
                                    null)))
               (list top-level-cli)))


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
     (with-flags flags
       (define proc
         (match action
           ["make" make-command]
           ["install" install-command]
           ["uninstall" uninstall-command]
           ["show" show-command]
           ["link" link-command]
           ["config" config-command]
           ["sandbox" sandbox-command]
           ["bundle" bundle-command]
           [_ (const (output-failure ($unrecognized-command action)))]))
       (proc args)))

#<<EOF
<action> is one of
  make       Create packages
  install    Install packages
  uninstall  Uninstall packages
  show       Print helpful information
  link       Create symlink to package file
  config     Manage configuration
  sandbox    Start sandboxed REPL for package's setup module.
  bundle     Prepare package for distribution

EOF
))



(define (make-command args)
  (run-command-line
   #:program "make"
   #:args args
   #:arg-help-strings '("package-defn")
   #:flags
   (settings->flag-specs
    XIDEN_SERVICE_ENDPOINTS
    XIDEN_DOWNLOAD_MAX_REDIRECTS
    XIDEN_INSTALL_RELATIVE_PATH
    XIDEN_TRUST_BAD_DIGEST
    XIDEN_TRUST_BAD_SIGNATURE
    XIDEN_TRUST_UNSIGNED
    XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS
    XIDEN_ALLOW_UNSUPPORTED_RACKET)

   (λ (flags . package-defn-input-exprs)
     (with-flags flags
       (output-fold package-defn-input-exprs
                    (list in-user-requested-package-definitions
                          in-package-modules))))))


(define (install-command args)
  (run-command-line
   #:program "install"
   #:args args
   #:arg-help-strings '("package-path")
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

   (λ (flags . package-paths)
     (with-flags flags
       (output-fold package-paths
                    (list #;in-user-specified-packages))))))


(define (link-command args)
  (run-command-line
   #:program "link"
   #:args args
   #:arg-help-strings '("link-path" "query" "rel-path")
   (λ (flags link-path query rel-path)
     (output-fold query
                  null #;(list find-exactly-one-installed-package
                        (λ (package-directory)
                          (make-link-to-package-file package-directory
                                                     link-path
                                                     rel-path)))))))

#|
     (define seq '(TODO: get package from query))
     (make-file-or-directory-link
      (build-path (with-handlers ([exn:fail?
                                    (λ (e)
                                      (write-output ($link-command-no-package query))
                                      (halt 1))])
                     (sequence-ref seq 0))
                  rel-path)
      link-path)
     (halt 0))))
|#


(define (config-command args)
  (define (make-fail-thunk str)
    (λ ()
      (output-return #:stop-value 1
                     #f
                     ($config-command-nonexistant-setting str))))

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
         (λ (flags key)
           (output-success ($show-setting-value (string->symbol key)))))]

       ["dump"
        (run-command-line
         #:args args
         #:program "config-dump"
         #:arg-help-strings '()
         (λ (flags)
           (output-success ($show-all-settings))))]

       ["set"
        (run-command-line
         #:args args
         #:program "config-set"
         #:arg-help-strings '("key" "value")
         (λ (flags key value)
           (apply-user-setting (string->symbol key) value)))]

       [_
        (output-return #:stop-value 1
                       ($unrecognized-command action))]))

   #<<EOF
<action> is one of
  set   Set a new value for a setting
  get   Get the current value of a setting
  dump  Write a hash table of all settings

EOF
))


(define (bundle-command args)
  (void))

#|
  (run-command-line
   #:program "bundle"
   #:arg-help-strings '("private-key-path")
   #:flags
   (settings->flag-specs XIDEN_PRIVATE_KEY_PATH)
   #:args args
   (λ (flags package-def-path file-patterns)
     (output-fold (string->path package-def-path)
                  (list read-package-definition
                        (λ (pkginfo) (bundle-package file-patterns))))

     (define metadata-file "package-definition.rkt")
     (define archive-file "source-code.tar")

     (define archive
       (parameterize ([current-directory package-path-string])
         (define archive-files
           (sequence->list
            (in-matching-files
             (map pregexp (if (XIDEN_MATCH_RACKET_MODULES)
                              (cons "\\.(ss|rkt|rktd|scrbl)$" pattern-strings)
                              pattern-strings))
             (current-directory))))

         (when (null? archive-files)
           (write-output ($no-files-match))
           (halt 1))

         (call-with-output-file archive-file
           (λ (to-archive-file)
             (pack archive-files to-archive-file)
             archive-file))))

     (define digest
       (with-handlers ([exn:fail:xiden:openssl?
                        (λ (e)
                          (write-output ($cannot-make-bundle-digest (exn:fail:xiden:openssl-exit-code e)))
                          (halt 1))])
         (make-digest archive 'sha384)))

     (define signature
       (with-handlers ([exn:fail:xiden:openssl?
                        (λ (e)
                          (write-output ($cannot-make-bundle-signature (exn:fail:xiden:openssl-exit-code e)))
                          (halt 1))])
         (if (XIDEN_PRIVATE_KEY_PATH)
             (make-signature digest (XIDEN_PRIVATE_KEY_PATH))
             #f)))

     (save-config! (make-config-closure
                    (package-info->hash info)
                    null)
                   metadata-file)

     (halt 0))))
|#




(define (sandbox-command args)
  (run-command-line
   #:program "sandbox"
   #:arg-help-strings '("query")

   #:flags
   (settings->flag-specs
    XIDEN_SANDBOX_MEMORY_LIMIT_MB
    XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB
    XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS
    XIDEN_SANDBOX_PATH_PERMISSIONS)

   #:args args
   (λ (flags query)
     (enter-package (void)))))


(define (show-command args)
  (run-command-line
   #:args args
   #:program "show"
   #:arg-help-strings '("what")
   (λ (flags what)
     (match what
       ["workspace"
        (output-return #:stop-value 0
                       #f
                       ($show-string ((workspace-directory))))]
       ["installed"
        (define in-installed '(TODO database query))
        (output-return
         #:stop-value 0
         #f
         (for/list ([info in-installed])
          (void)))]
       [_
        (output-return #:stop-value 1 #f ($unrecognized-command what))]))
   #<<EOF
where <what> is one of
  workspace  The current target workspace directory
  installed  A list of installed packages, as queries

EOF
))


(define (uninstall-command args)
  (run-command-line
   #:program "uninstall"
   #:arg-help-strings '("query")
   #:flags
   (settings->flag-specs XIDEN_CONSENT)
   #:args args
   (λ (flags . queries)
     (void))))



; Base bindings follow

(define (run-command-line #:program program
                          #:flags [flags null]
                          #:args [args (current-command-line-arguments)]
                          #:arg-help-strings arg-help-strings
                          #:suffix-is-index? [suffix-is-index? #t]
                          handle-arguments
                          [help-suffix ""])
  ; This is helpful for functional tests since it enables vanilla quasiquoting.
  (define argv
    (if (list? args)
        (list->vector args)
        args))

  (define help-requested?
    (or (vector-member "-h" argv)
        (vector-member "--help" argv)))

  ; parse-command-line does not show help when arguments are missing
  ; and -h is not set.  Show help anyway.
  (define (show-help-on-zero-arguments e)
    (output-return #:stop-value 1
                   ($show-string
                    (format "~a~n~a"
                            (exn-message e)
                            (if (and (regexp-match? #px"given 0 arguments" (exn-message e))
                                     suffix-is-index?
                                     (not help-requested?))
                                help-suffix
                                "")))))


  (call/cc
   ; The callback for showing the help string does not stop evaluation
   ; of the argument handler. This is why parse-command-line calls the
   ; exit handler by default. Use a continuation to maintain a
   ; functional approach.
   (λ (force-output)
     (with-handlers ([exn:fail:user? show-help-on-zero-arguments])
       (parse-command-line program argv
                           (if (null? flags)
                               null
                               `((once-each . ,flags)))
                           handle-arguments
                           arg-help-strings
                           (λ (help-str)
                             (force-output
                              (output-return #:stop-value (if help-requested? 0 1)
                                             #f
                                             ($show-string (format "~a~n~a" help-str help-suffix))))))))))

; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
(module+ test
  (require racket/port
           racket/random
           rackunit
           (submod "file.rkt" test))

  (test-case "Configure xiden"
    (test-workspace "Respond to an incomplete command with help"
                    (define out (entry-point '("config")))
                    (define help-message (findf $show-string? ($with-output-accumulated out)))
                    (check-eq? ($with-output-stop-value out) 1)
                    (check-true
                     (andmap (λ (patt) (regexp-match? (regexp patt) ($show-string-message help-message)))
                             '("given 0 arguments"
                               "set"
                               "get"
                               "dump"))))

    (test-workspace "Dump all (read)able configuration on request"
                    (entry-point '("config" "dump")

                       (λ (exit-code stdout stderr output)
                         (check-eq? exit-code 0)
                         (check-equal? (dump-xiden-settings)))))

    (test-workspace "Return a (read)able config value"
      (define config-key (random-ref (in-hash-keys XIDEN_SETTINGS)))
      (define config-key/str (symbol->string config-key))
      (entry-point (vector "config" "get" config-key/str)
                       (λ (exit-code stdout stderr output)
                         (check-eq? exit-code 0)
                         (check-equal? (get-xiden-setting-value config-key)
                                       (read stdout)))))

    (test-workspace "Save a (write)able config value"
      ; This confirms that a new workspace has different results.
      (check-false (XIDEN_VERBOSE))
      (check-false (file-exists? (get-xiden-settings-path)))
      (entry-point '("config" "get" "XIDEN_VERBOSE"))
      (entry-point '("config" "set" "XIDEN_VERBOSE" "#t")))))
