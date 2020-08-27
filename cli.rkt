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
         "input-forms-lang.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "package.rkt"
         "package-info.rkt"
         "printer.rkt"
         "query.rkt"
         "rc.rkt"
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
  (match-define ($with-messages stop-value accumulated)
    (entry-point (current-command-line-arguments)))
  (sequence-for-each write-output accumulated)
  (exit stop-value))

; Define a transition from accumulated command line flags to a new
; parameterization in terms of those flags and a cached read of the
; rcfile. Capture any failure in this transition as main program
; output.
(define-syntax-rule (with-rc flags body ...)
  (with-handlers ([exn:fail? (λ (e) (attach-message 0 ($fail (exn-message e))))])
    (with-xiden-rcfile (call-with-applied-settings flags (λ () body ...)))))


; Keep seperate for functional tests.
(define (entry-point args)
  (:do #:with (:return args)
       (λ (args)
         (if (show-workspace-envvar-error?)
             (attach-message args ($invalid-workspace-envvar))
             (:return args)))
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
     (with-rc flags
       (with-handlers ([$with-messages? values])
         (for/fold ([output (:return)])
                   ([source (in-list pkgdef-sources)])
           (:merge output
                   (install-package-from-source source))))))))


(define (uninstall-command args)
  (run-command-line
   #:program "uninstall"
   #:arg-help-strings '("query")
   #:flags
   (settings->flag-specs XIDEN_CONSENT)
   #:args args
   (λ (flags . queries)
     (void))))


(define (link-command args)
  (run-command-line
   #:program "link"
   #:args args
   #:arg-help-strings '("link-path" "query" "rel-path")
   (λ (flags link-path query rel-path)
     (define maybe-package-id (find-latest-package-id (coerce-xiden-query query)))
     (if maybe-package-id
         (begin (make-file-or-directory-link (build-path (get-derivation-directory maybe-package-id)
                                                         rel-path)
                                             link-path)
                (:return 0))
         (attach-message 1 ($no-package-found))))))


(define (config-command args)
  (define (get-setting name)
    (define maybe-selected-setting (setting-ref name))
    (if maybe-selected-setting
        (:return maybe-selected-setting)
        (attach-message #f ($no-such-setting))))

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
               (attach-message 1 ($no-such-setting)))))]


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
                                                  ($reject-user-setting
                                                   (setting-id maybe-selected-setting)
                                                   value-string
                                                   (if (exn:fail:contract? e)
                                                       (cadr (regexp-match #px"expected:\\s+([^\n]+)"
                                                                           (exn-message e)))
                                                       (exn-message e)))))])
                 (attach-message 0
                                 (maybe-selected-setting (read (open-input-string value-string))
                                                         save-xiden-settings!)))
               (attach-message 1 ($no-such-setting)))))]

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
    XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS
    XIDEN_SANDBOX_PATH_PERMISSIONS)

   #:args args
   (λ (flags query)
     (parameterize ([current-eval (make-sandbox (void))])
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
    (attach-message 1
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
                              (:return #:stop-value (if help-requested? 0 1)
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
