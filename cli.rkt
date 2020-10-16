#lang racket/base

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
         "archiving.rkt"
         "cli-flag.rkt"
         "cmdline.rkt"
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
         "sentry.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "team.rkt"
         "transaction.rkt"
         "url.rkt"
         "openssl.rkt"
         "worker.rkt"
         "workspace.rkt")

(module+ main
  (exit (entry-point (current-command-line-arguments)
                     (get-message-formatter)
                     top-level-cli)))

(define (top-level-cli args halt)
  (run-command-line
   #:program "xiden"
   #:arg-help-strings '("action" "args")
   #:help-suffix-string-key 'top-level-cli-help
   #:args args
   #:halt halt
   #:flags
   (make-cli-flag-table --fasl-output
                        --reader-friendly-output
                        --verbose)
   (λ (flags action . args)
     (with-rc flags
       (define proc
         (match action
           ["do" do-command]
           ["show" show-command]
           ["gc" gc-command]
           [_ (const (halt 1 ($cli:undefined-command action)))]))
       (proc args halt)))))


(define (do-command args halt)
  (run-command-line
   #:program "do"
   #:args args
   #:halt halt
   #:arg-help-strings '()
   #:flags
   (make-cli-flag-table ++install-source
                        ++install-abbreviated
                        ++install-default
                        ++trust-public-key
                        ++envvar
                        ++bin
                        ++host
                        --max-redirects
                        --trust-any-digest
                        --trust-any-pubkey
                        --trust-bad-signature
                        --trust-unsigned
                        --allow-undeclared-racket
                        --assume-support
                        --sandbox-memory-limit
                        --sandbox-eval-memory-limit
                        --sandbox-eval-time-limit)
   (λ (flags)
     (with-rc flags
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
                         (λ (messages) (rollback) (halt 1 messages))))))))))


(define (gc-command args halt)
  (run-command-line
   #:program "gc"
   #:args args
   #:halt halt
   #:arg-help-strings '()
   (λ (flags)
     (halt 0 ($finished-collecting-garbage (xiden-collect-garbage))))))


(define (show-command args halt)
  (run-command-line
   #:args args
   #:halt halt
   #:help-suffix-string-key 'show-command-help
   #:program "show"
   #:arg-help-strings '("what")
   (λ (flags what)
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
        (halt 1 ($cli:undefined-command what))]))))




; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
(module+ test
  (require racket/port
           racket/random
           rackunit
           (submod "file.rkt" test))

  (test-workspace "Install a package"
    (define defn
      '(module pkg xiden
         (define inputs null)
         (define outputs '("lib"))
         (define (build output)
           (void))))

    (void))

  (test-case "Dump all (read)able configuration on request"
    (show-command '("config")
                  (λ (exit-code msg)
                    (check-eq? exit-code 0)
                    (check-equal? msg ($show-datum (dump-xiden-settings)))))))
