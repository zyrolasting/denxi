#lang racket/base

; Define primary entry point for the program.

(provide launch-xiden!)

(require racket/match
         racket/path
         racket/sequence
         "cli-flag.rkt"
         "cmdline.rkt"
         "codec.rkt"
         "crypto.rkt"
         "format.rkt"
         "input.rkt"
         "integrity.rkt"
         "l10n.rkt"
         "state.rkt"
         "subprogram.rkt"
         "message.rkt"
         "monad.rkt"
         "notary.rkt"
         "package.rkt"
         "pkgdef/static.rkt"
         "port.rkt"
         "printer.rkt"
         "query.rkt"
         "racket-module.rkt"
         "security.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "transaction.rkt")


(module+ main (launch-xiden!))

(define (launch-xiden! #:arguments [args (current-command-line-arguments)]
                       #:format-message [format-message (get-message-formatter)]
                       #:handle-exit [handle-exit exit])
  (run-entry-point! args
                    format-message
                    top-level-cli
                    handle-exit))


(define (top-level-cli args)
  (cli #:program "xiden"
       #:arg-help-strings '("action" "args")
       #:help-suffix-string-key 'top-level-cli-help
       #:args args
       #:flags
       (make-cli-flag-table
        ++envvar
        ++trust-executable
        ++trust-host-executable
        ++trust-cert
        --workspace
        --fasl-output
        --memory-limit
        --reader-friendly-output
        --time-limit
        --trust-any-exe
        --trust-any-host
        --verbose)
       (λ (flags action . remaining-args)
         (define-values (restrict? name proc)
           (match action
             ["do" (values #t "do" do-command)]
             ["show" (values #t "show" show-command)]
             ["gc" (values #t "gc" gc-command)]
             ["mkint" (values #t "mkint" mkint-command)]
             ["fetch" (values #t "fetch" fetch-command)]
             [_ (values ""
                        (λ _ (values null
                                     (λ (halt)
                                       (halt 1 ($cli:undefined-command action))))))]))
         (define-values (subflags planned) (proc remaining-args))
         (values (append flags subflags)
                 (λ (halt)
                   (if restrict?
                       (restrict halt
                                 planned
                                 #:memory-limit (XIDEN_MEMORY_LIMIT_MB)
                                 #:time-limit (XIDEN_TIME_LIMIT_S)
                                 #:trusted-executables (XIDEN_TRUST_EXECUTABLES)
                                 #:allowed-envvars (XIDEN_ALLOW_ENV)
                                 #:trust-unverified-host? (XIDEN_TRUST_UNVERIFIED_HOST)
                                 #:trust-any-executable? (XIDEN_TRUST_ANY_EXECUTABLE)
                                 #:trust-certificates (XIDEN_TRUST_CERTIFICATES)
                                 #:implicitly-trusted-host-executables (XIDEN_TRUST_HOST_EXECUTABLES)
                                 #:workspace (XIDEN_WORKSPACE)
                                 #:gc-period 30
                                 #:name name)
                       (planned halt)))))))


(define (do-command args)
  (cli #:program "do"
       #:args args
       #:arg-help-strings '()
       #:flags
       (make-cli-flag-table ++install-source
                            ++install-abbreviated
                            ++install-default
                            ++trust-public-key
                            ++trust-chf
                            ++input-override
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


                     ["log"
                      (let loop ([next (read)])
                        (unless (eof-object? next)
                          (if ($message? next)
                              (write-message next)
                              (writeln next))
                          (loop (read))))
                      (halt 0 null)]

                     ["links"
                      (halt 0
                            (sequence->list
                             (sequence-map (λ (link-path target-path)
                                             ($show-datum (cons link-path target-path)))
                                           (in-issued-links))))]

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



(define-namespace-anchor cli-namespace-anchor)
(define (fetch-command args)
  (cli #:args args
       #:program "fetch"
       #:arg-help-strings '("source-expr")
       #:flags
       (make-cli-flag-table --fetch-total-size
                            --fetch-buffer-size
                            --fetch-pkgdef-size
                            --fetch-timeout
                            --max-redirects)
       (λ (flags source-expr-string)
         (define display-name
           (~s (~a #:max-width 60 #:limit-marker "..." source-expr-string)))

         (define (copy-to-stdout in est-size)
           (transfer in
                     (current-output-port)
                     #:on-status
                     (λ (status-message)
                       (write-message status-message
                                      (current-message-formatter)
                                      (current-error-port)))
                     #:transfer-name display-name
                     #:max-size (mebibytes->bytes (XIDEN_FETCH_TOTAL_SIZE_MB))
                     #:buffer-size (mebibytes->bytes (XIDEN_FETCH_BUFFER_SIZE_MB))
                     #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
                     #:est-size est-size))

         (values flags
                 (λ (halt)
                   (define unnormalized-datum
                     (with-handlers ([exn?
                                      (λ (e)
                                        ((error-display-handler) (exn-message e) e)
                                        (halt 1 null))])
                       (string->value source-expr-string)))


                   (define datum
                     (cond [(symbol? unnormalized-datum)
                            (coerce-source (~a unnormalized-datum))]
                           [(string? unnormalized-datum)
                            (coerce-source unnormalized-datum)]
                           [else unnormalized-datum]))

                   (define program
                     (mdo source :=
                          (eval-untrusted-source-expression
                           datum
                           (namespace-anchor->namespace cli-namespace-anchor))
                          (subprogram-fetch display-name source copy-to-stdout)))

                   (define-values (result messages) (run-subprogram program))
                   (parameterize ([current-output-port (current-error-port)])
                     (write-message-log messages (current-message-formatter)))
                   (halt (if (eq? result FAILURE) 1 0) null))))))


; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
(module+ test
  (provide
   (all-from-out racket
                 racket/runtime-path
                 rackunit)
   (contract-out
    [test-cli
     (-> string?
         (or/c (listof string?)
               (vectorof string?))
         (-> exit-code/c
             program-log/c
             bytes?
             bytes?
             any)
         any)]
    [check-cli
     (-> (or/c (listof string?)
               (vectorof string?))
         (-> exit-code/c
             program-log/c
             bytes?
             bytes?
             any)
         any)]
    [check-garbage-collection
     (-> predicate/c void?)]))

  (require racket
           racket/runtime-path
           rackunit
           "file.rkt"
           (submod "state.rkt" test))

  (define mkflag shortest-cli-flag)

  (define (check-cli args continue)
    (define messages null)
    (define formatter (get-message-formatter))
    (define stdout (open-output-bytes))
    (define stderr (open-output-bytes))
    (call-with-snake-oil-cipher-trust
     (λ ()
       (call-with-values
        (λ ()
          (parameterize ([current-output-port stdout]
                         [current-error-port stderr])
            (launch-xiden! #:arguments args
                           #:format-message
                           (λ (m)
                             (set! messages (cons m messages))
                             (formatter m))
                           #:handle-exit
                           (λ (status)
                             (values status
                                     (reverse messages)
                                     (get-output-bytes stdout #t)
                                     (get-output-bytes stderr #t))))))
        continue))))


  (define (split-buffer-lines buf)
    (string-split (bytes->string/utf-8 buf) "\n"))

  (define (test-cli msg args continue)
    (test-case msg (check-cli args continue)))


  (define (check-garbage-collection ok?)
    (check-cli (list "gc")
               (λ (exit-code messages stdout stderr)
                 (check-equal? exit-code 0)
                 (match messages
                   [(list ($finished-collecting-garbage r)) (check-pred ok? r)]
                   [_ (fail (format (~a "Garbage collection command "
                                        "returned unexpected messages:~n~s")
                                    messages))]))))

  (test-cli "Fetch from user-provided sources"
            '("fetch" "(byte-source #\"abcdef\")")
            (λ (exit-code messages stdout stderr)
              (check-equal? stdout #"abcdef")
              (check-equal? exit-code 0)
              (check-true (> (bytes-length stderr) 0))))


  (test-case "Echo logs"
    (parameterize ([current-input-port (open-input-bytes #"1 #s(($show-string $message 0) \"a\") 2")])
      (check-cli (list "show" "log")
                 (λ (exit-code messages stdout stderr)
                   (check-equal? exit-code 0)
                   (check-equal? stderr #"")
                   (check-equal? stdout #"1\na\n2\n"))))))
