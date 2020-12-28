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
         (only-in net/url-connect current-https-protocol)
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
         "sandbox.rkt"
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
       (make-cli-flag-table --fasl-output
                            --reader-friendly-output
                            --verbose)
       (λ (flags action . remaining-args)
         (define proc
           (match action
             ["do" do-command]
             ["show" show-command]
             ["gc" gc-command]
             ["mkint" mkint-command]
             [_ (λ _ (values null
                             (λ (halt)
                               (halt 1 ($cli:undefined-command action)))))]))

         (define-values (subflags planned) (proc remaining-args))
         (values (append flags subflags)
                 planned))))


(define (do-command args)
  (cli #:program "do"
       #:args args
       #:arg-help-strings '()
       #:flags
       (make-cli-flag-table ++install-source
                            ++install-abbreviated
                            ++install-default
                            ++trust-public-key
                            ++trust-executable
                            ++envvar
                            ++host
                            --fetch-total-size
                            --fetch-buffer-size
                            --fetch-pkgdef-size
                            --max-redirects
                            --trust-any-digest
                            --trust-any-exe
                            --trust-any-pubkey
                            --trust-bad-signature
                            --trust-unsigned
                            --assume-support
                            --sandbox-memory-limit
                            --sandbox-eval-memory-limit
                            --sandbox-eval-time-limit)
       (λ (flags)
         (values flags
                 (λ (halt)
                   (define actions
                     (fold-transaction-actions
                      flags
                      (hasheq XIDEN_INSTALL_ABBREVIATED_SOURCES
                              (match-lambda [source
                                             (run-package source)])
                              XIDEN_INSTALL_DEFAULT_SOURCES
                              (match-lambda [(list link-path source)
                                             (run-package source
                                                          #:link-path link-path)])
                              XIDEN_INSTALL_SOURCES
                              (match-lambda [(list link-path output-name source)
                                             (run-package source
                                                          #:output-name output-name
                                                          #:link-path link-path)]))))

                   (if (null? actions)
                       (halt 0 null)
                       (let-values ([(commit rollback) (start-transaction!)])
                         (parameterize ([current-https-protocol (if (XIDEN_TRUST_UNVERIFIED_HOST) 'auto 'secure)])
                           (transact actions
                                     (λ (messages) (commit)   (halt 0 messages))
                                     (λ (messages) (rollback) (halt 1 messages)))))))))))


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
                                                       (xiden-query->string
                                                        (xiden-query provider
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
