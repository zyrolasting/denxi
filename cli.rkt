#lang racket/base

; Define primary entry point for the program.

(require racket/function
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
         racket/stream
         racket/vector
         (for-syntax racket/base)
         "cli-flag.rkt"
         "cmdline.rkt"
         "codec.rkt"
         "contract.rkt"
         "exn.rkt"
         "file.rkt"
         "format.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "l10n.rkt"
         "localstate.rkt"
         "message.rkt"
         "monad.rkt"
         "package.rkt"
         "printer.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "security.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "transaction.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt")

(module+ main
  (run-entry-point! (current-command-line-arguments)
                    (get-message-formatter)
                    top-level-cli
                    exit))


(define (top-level-cli args)
  (cli #:program "xiden"
       #:arg-help-strings '("action" "args")
       #:help-suffix-string-key 'top-level-cli-help
       #:args args
       #:flags
       (make-cli-flag-table
        ++envvar
        ++trust-executable
        --fasl-output
        --memory-limit
        --reader-friendly-output
        --time-limit
        --trust-any-exe
        --trust-any-host
        --verbose)
       (λ (flags action . remaining-args)
         (define-values (name proc)
           (match action
             ["do" (values "do" do-command)]
             ["show" (values "show" show-command)]
             ["gc" (values "gc" gc-command)]
             ["mkint" (values "mkint" mkint-command)]
             [_ (values ""
                        (λ _ (values null
                                     (λ (halt)
                                       (halt 1 ($cli:undefined-command action))))))]))
         (define-values (subflags planned) (proc remaining-args))
         (values (append flags subflags)
                 (λ (halt)
                   (restrict halt
                             planned
                             #:memory-limit (XIDEN_MEMORY_LIMIT_MB)
                             #:time-limit (XIDEN_TIME_LIMIT_S)
                             #:trusted-executables (XIDEN_TRUSTED_EXECUTABLES)
                             #:allowed-envvars (XIDEN_ALLOW_ENV)
                             #:trust-unverified-host? (XIDEN_TRUST_UNVERIFIED_HOST)
                             #:trust-any-executable? (XIDEN_TRUST_ANY_EXECUTABLE)
                             #:workspace (workspace-directory)
                             #:gc-period 30
                             #:name name))))))


(define (do-command args)
  (cli #:program "do"
       #:args args
       #:arg-help-strings '()
       #:flags
       (make-cli-flag-table ++install-source
                            ++install-abbreviated
                            ++install-default
                            ++trust-public-key
                            ++host
                            --fetch-total-size
                            --fetch-buffer-size
                            --fetch-pkgdef-size
                            --max-redirects
                            --trust-any-digest
                            --trust-any-pubkey
                            --trust-bad-signature
                            --trust-unsigned
                            --assume-support)
       (λ (flags)
         (values flags
                 (λ (halt)
                   (define actions
                     (fold-transaction-actions
                      flags
                      (hasheq XIDEN_INSTALL_ABBREVIATED_SOURCES
                              (match-lambda [source
                                             (install #f #f source)])
                              XIDEN_INSTALL_DEFAULT_SOURCES
                              (match-lambda [(list link-path source)
                                             (install link-path #f source)])
                              XIDEN_INSTALL_SOURCES
                              (match-lambda [(list link-path output-name source)
                                             (install link-path output-name source)]))))
                   (if (null? actions)
                       (halt 0 null)
                       (let-values ([(commit rollback) (start-transaction!)])
                         (transact actions
                                   (λ (messages) (commit)   (halt 0 messages))
                                   (λ (messages) (rollback) (halt 1 messages))))))))))


(define (gc-command args)
  (cli #:program "gc"
       #:args args
       #:arg-help-strings '()
       (λ (flags)
         (values flags
                 (λ (halt)
                   (halt 0 ($finished-collecting-garbage (xiden-collect-garbage))))))))


(define (show-command args)
  (cli #:args args
       #:help-suffix-string-key 'show-command-help
       #:program "show"
       #:arg-help-strings '("what")
       (λ (flags what)
         (values flags
                 (λ (halt)
                   (match what
                     ["config"
                      (halt 0 ($show-datum (dump-xiden-settings)))]

                     ["installed"
                      (halt 0
                            (sequence->list
                             (sequence-map
                              (match-lambda*
                                [(list _ provider _ package _ edition _ revision _ output _ path)
                                 ($show-string (format "~a ~a ~a"
                                                       (format-parsed-package-query
                                                        (parsed-package-query provider
                                                                              package
                                                                              edition
                                                                              (~a revision)
                                                                              (~a revision)
                                                                              "ii"))
                                                       output
                                                       (file-name-from-path path)))])
                              (in-all-installed))))]



                     ["links"
                      (halt 0
                            (sequence->list
                             (sequence-map (λ (link-path target-path)
                                             ($show-string (format "~a -> ~a" link-path target-path)))
                                           (in-issued-links))))]

                     ["workspace"
                      (halt 0 ($show-string (path->string (workspace-directory))))]

                     [_
                      (halt 1 ($cli:undefined-command what))]))))))


(define (mkint-command args)
  (cli #:args args
       #:program "mkint"
       #:arg-help-strings '("algorithm" "encoding" "file")
       (λ (flags algorithm-str encoding-str file-or-stdin)
         (values flags
                 (λ (halt)
                   (with-handlers ([exn? (λ (e) (raise-user-error 'mkint (exn-message e)))])
                     (let ([algo (string->symbol algorithm-str)]
                           [encoding (string->symbol encoding-str)]
                           [port
                            (if (equal? "-" file-or-stdin)
                                (current-input-port)
                                (open-input-file file-or-stdin))])
                       (halt 0
                             ($show-datum
                              `(integrity
                                ',algo
                                (,(if (member encoding '(hex colon-separated-hex))
                                      'hex
                                      encoding)
                                 ,(coerce-string
                                   (encode encoding (dynamic-wind void
                                                                  (λ ()
                                                                    (make-digest port algo))
                                                                  (λ ()
                                                                    (close-input-port port))))))))))))))))



; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
(module+ test
  (require racket/port
           racket/random
           rackunit
           (submod "file.rkt" test))

  (test-case "Dump all (read)able configuration on request"
    (call-with-values (λ () (show-command '("config")))
                      (λ (flags run!)
                        (check-pred null? flags)
                        (run!
                         (λ (exit-code msg)
                           (check-equal? exit-code 0)
                           (check-equal? msg ($show-datum (dump-xiden-settings)))))))))
