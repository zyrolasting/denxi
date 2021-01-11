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
         "logged.rkt"
         "message.rkt"
         "monad.rkt"
         "package.rkt"
         "pkgdef/static.rkt"
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
        ++trust-host-executable
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
             ["mkinput" (values "mkinput" mkinput-command)]
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
                             #:implicitly-trusted-host-executables (XIDEN_TRUSTED_HOST_EXECUTABLES)
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


(define (mkinput-command args)
  (cli #:args args
       #:program "mkinput"
       #:arg-help-strings '("path")
       #:flags
       (make-cli-flag-table
        ++user-facing-source
        --byte-encoding
        --generated-input-name
        --md
        --signer)
       (λ (flags file-path)
         (values flags
                 (λ (halt)
                   (match-define (list pubkey prvkey pass) (XIDEN_SIGNER))
                   (define default-name (XIDEN_GENERATED_INPUT_NAME))
                   (define byte-encoding (XIDEN_BYTE_ENCODING))
                   (define md-algorithm (XIDEN_MESSAGE_DIGEST_ALGORITHM))
                   (define user-facing-sources (reverse (XIDEN_USER_FACING_SOURCES)))
                   (halt 0
                         ($show-datum
                          (autocomplete-input-expression
                           #:byte-encoding byte-encoding
                           #:default-md-algorithm md-algorithm
                           #:private-key-password-path pass
                           #:default-name default-name
                           #:find-file (λ _ file-path)
                           #:private-key-path prvkey
                           #:public-key-source pubkey
                           `(input ,default-name
                                   (sources . ,user-facing-sources))))))))))



; Functional tests follow. Use to detect changes in the interface and
; verify high-level impact.
(module+ test
  (require racket/port
           racket/random
           racket/runtime-path
           rackunit
           (submod "file.rkt" test))

  (define mkflag shortest-cli-flag)

  (define-runtime-path private/ "private")

  (define (make-test-path . args)
    (~a (path->complete-path (apply build-path private/ args))))

  (define test-private-key-path (make-test-path "privkey.pem"))
  (define test-private-key-password-path (make-test-path "pass"))
  (define test-dummy-file-path (make-test-path "dummy"))
  (define test-public-key-path (make-test-path "pubkey.pem"))

  (define (run-functional-test prog)
    (call-with-values prog
                      (λ (flags run!)
                        (call-with-bound-cli-flags
                         flags (λ ()
                                 (define-values (exit-code msg) (call/cc run!))
                                 (values flags exit-code msg))))))

  (define-syntax-rule (try prog)
    (run-functional-test (λ () prog)))

  (test-case "Dump all (read)able configuration on request"
    (define-values (flags exit-code msg) (try (show-command '("config"))))
    (check-pred null? flags)
    (check-equal? exit-code 0)
    (check-equal? msg ($show-datum (dump-xiden-settings))))

  (test-case "Generate input expressions"
    (define (expected-simple-input . sources)
      `(input ,DEFAULT_STRING
              (sources . ,sources)
              (integrity 'sha384
                         (base64 "l0kWCDBh9kC+NzqGXwGTdELJESIB+JUHx1f1dDQ5IFyjpZHoQyocK6lMslXieV+B"))))

    (test-case "Allow empty sources"
      (define-values (flags exit-code msg) (try (mkinput-command (list test-dummy-file-path))))
      (check-pred null? flags)
      (check-equal? exit-code 0)
      (check-equal? msg ($show-datum (expected-simple-input))))

    (test-case "Generate integrity expression with default values"
      (define-values (flags exit-code msg)
        (try (mkinput-command (list (mkflag +u) "user-source1"
                                    (mkflag +u) "user-source2"
                                    test-dummy-file-path))))

      (check-pred cli-flag-state? (car flags))
      (check-equal? exit-code 0)
      (check-equal? msg ($show-datum (expected-simple-input "user-source1" "user-source2"))))

    (test-case "Generate all specified values"
      (define md-algo 'md5)
      (define expected-digest-base32 "0rsd0k4es4dks32empq1we4gh8======")
      (define expected-digest-expr `(base32 ,expected-digest-base32))
      (define expected-digest (base32 expected-digest-base32))
      (define public-key-source "https://example.com/pubkey")
      (define signature-base32
        (string-join
         '("q00q65n5tja2bdqgkv03qjwt3rapw2zcwxtf98pm8rtj26gdkgb2"
           "fmax95kjbe31mq5knk9c22rretn4ftx57axgp5ca477wt23ec2tq"
           "httqb3yjnqabq2x0t8qrr2zww4fk3m3q3a2rbg23arhv3h9dygv8"
           "xeq0vv67xx8zgvd58ecq3jxw8eq14x6kefpg94btpk1c1b5qf28b"
           "79cfkptx6vbaertbm32xc7aeedza68ta3wyjrk7z9jk8wctz22zb"
           "v8zjrea33vczmp39j30pngs9rk1hyt1mq6t16xxmkn7hhgkqqy7m"
           "ays1kx3mhg8q6hqmetkcvkfwmcve6mj9ewxe9ms14wttzns0hcak"
           "xp8cf6j35kv3ametscrq6jb3peyz6tr01nfed6g8ab5kaj4nr9h1"
           "31dcdxmee3j1mtp3jbectf3a5qb9fr7x0htytf6j3r3ymcaqjthr"
           "akdbkr5jt2pabxmw5j038b42a3neb4t6psky36p1x03yh194shk6"
           "we9mmn3d0ermfamxstrm8t96w62n86nvzjbhwchysbgmwtbenrga"
           "aez451szz0er297cv1wf3715wx0ff2r8b0z5acee33g=")
         ""))

      (define expected-integrity-info (integrity md-algo (base32 expected-digest-base32)))
      (define expected-signature-info (signature public-key-source (base32 signature-base32)))

      (define-values (flags exit-code msg)
        (try (mkinput-command (list (mkflag ++user-facing-source) "user-source1"
                                    (mkflag ++user-facing-source) "user-source2"
                                    (mkflag --byte-encoding) "base32"
                                    (mkflag --generated-input-name) "boo"
                                    (mkflag --md) "md5"
                                    (mkflag --signer)
                                    public-key-source
                                    test-private-key-path
                                    test-private-key-password-path
                                    test-dummy-file-path))))

      (check-pred cli-flag-state? (car flags))
      (check-equal? exit-code 0)
      (check-equal? msg
                    ($show-datum
                     `(input "boo"
                             (sources "user-source1" "user-source2")
                             (integrity ',md-algo (base32 ,expected-digest-base32))
                             (signature ,public-key-source (base32 ,signature-base32)))))

      (test-true "Generate integrity expression that actually reflects content"
        ($integrity-ok?
         (check-integrity
          #:trust-bad-digest #f
          (integrity 'md5 (base32 expected-digest-base32))
          (build-path private/ "dummy"))))

      (test-true "Generate signature that matches the digest"
       ($signature-ok?
        (check-signature
         #:trust-public-key? (λ _ #t)
         #:public-key-path test-public-key-path
         #:trust-unsigned #f
         #:trust-bad-digest #f
         expected-signature-info
         expected-integrity-info))))))
