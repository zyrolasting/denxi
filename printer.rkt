#lang racket/base

; Define procedures to write program output to ports

(require racket/contract)
(provide
 (contract-out
  [write-output
   (->* ($message?) (output-port?) void?)]))

(require racket/fasl
         racket/pretty
         racket/serialize
         "format.rkt"
         "message.rkt"
         "package-info.rkt"
         "rc.rkt"
         "setting.rkt"
         "xiden-messages.rkt")


(define (filter-output m)
  (if ($verbose? m)
      (and (XIDEN_VERBOSE)
           ($verbose-message m))
      m))


(define (write-output v [out (current-output-port)])
  (define maybe-message (filter-output v))
  (when maybe-message
    (parameterize ([current-output-port out])
      (define to-send
        (if (XIDEN_READER_FRIENDLY_OUTPUT)
            maybe-message
            (format-xiden-message maybe-message)))

      (if (XIDEN_FASL_OUTPUT)
          (s-exp->fasl (serialize to-send) (current-output-port))
          (if (XIDEN_READER_FRIENDLY_OUTPUT)
              (pretty-write #:newline? #t to-send)
              (displayln to-send)))
      (flush-output))))


(define (format-package-info info)
  (package-info-package-name info))


(define (format-setting-flag-example s)
  (format "~a/~a"
          (setting-short-flag s)
          (setting-long-flag s)))


(define (format-xiden-message m)
  (cond [($output? m) (format-xiden-message ($output-v m))]

        [($fail? m)
         (define v ($fail-to-display m))
         (cond [(exn? v) (exn->string v)]
               [(string? v) v]
               [else (~s v)])]

        [($show-string? m)
         ($show-string-message m)]

        [($already-installed? m)
         (format "~a is already installed"
                 (format-package-info ($already-installed-info m)))]

        [($module-compiled? m)
         (format "Compiled: ~a"
                 ($module-compiled-module-path m))]

        [($compilation-error? m)
         (format "Bytecode compilation error in: ~a~n~a"
                 ($compilation-error-module-path m)
                 ($compilation-error-message m))]

        [($bad-digest? m)
         (format (~a "~a failed its integrity check.~n"
                     "While unsafe, you can force installation using ~a.")
                 (format-package-info ($bad-digest-info m))
                 (setting-long-flag XIDEN_TRUST_BAD_DIGEST))]

        [($mod-load-failure? m)
         (format (~a "Could not load plugin module ~a. Using default implementations.~n"
                     "Load error: ~a")
                 ($mod-load-failure-path m)
                 ($mod-load-failure-original-error-string m))]

        [($bad-signature? m)
         (format (~a "~a has a signature, but it does not match ~a's public key.~n"
                     "While unsafe, you can trust bad signatures using ~a.")
                 (format-package-info ($bad-signature-info m))
                 (package-info-provider-name ($bad-signature-info m))
                 (setting-long-flag XIDEN_TRUST_BAD_SIGNATURE))]

        [($missing-signature? m)
         (format (~a "~a does not have a signature. If you are testing a package, this is expected.~n"
                     "If you got the package from the Internet, then exercise caution!~n"
                     "To trust unsigned packages, use ~a.")
                 (format-package-info ($missing-signature-info m))
                 (setting-long-flag XIDEN_TRUST_UNSIGNED))]

        [($on-unverified-host? m)
         (format (~a "~a does not have a valid certificate.~n"
                     "Connections to this server are not secure.~n"
                     "To trust servers without valid certificates, use ~a.")
                 ($on-unverified-host-host m)
                 (setting-long-flag XIDEN_TRUST_UNVERIFIED_HOST))]

        [($package-installed? m)
         (format "Installed package ~a"
                 (format-package-info ($package-installed-info m)))]

        [($on-request? m)
         (format "~a ~a ~a client=~a host=~a referer=~a"
                 (parameterize ([date-display-format 'iso-8601])
                   (date->string (seconds->date ($on-request-timestamp m)) #t))
                 (string-upcase (bytes->string/utf-8 ($on-request-method m)))
                 ($on-request-uri m)
                 ($on-request-client-ip m)
                 ($on-request-host-ip m)
                 ($on-request-referer m))]

        [($declare-input? m)
         (format "New input ~a located at ~a"
                 ($declare-input-digest m)
                 ($declare-input-path m))]

        [($unrecognized-command? m)
         (format "Unrecognized command: ~s. Run with -h for usage information.~n"
                 ($unrecognized-command-command m))]

        [($consent-note? m)
         (format "To consent to these changes, run again with ~a"
                 (setting-short-flag XIDEN_CONSENT))]

        [($link-command-no-package? m)
         (format "Cannot find a package using ~s."
                 ($link-command-no-package-query-string m))]

        [($setup-command-no-package? m)
         (format "Cannot find a package using ~s."
                 ($setup-command-no-package-query-string m))]

        [($config-command-nonexistant-setting? m)
         (format "There is no setting called ~a.~n"
                 ($config-command-nonexistant-setting-name m))]

        [($init-localstate? m)
         (format "Initalizing local state at ~a"
                 ($init-localstate-path m))]

        [($no-package-inputs? m)
         "No package inputs specified."]

        [($after-write? m)
         (format "Wrote ~a" ($after-write-path m))]

        [($no-files-match? m)
         "The patterns specified did not match any files."]

        [($cannot-make-bundle-digest? m)
         (format "OpenSSL exited with code ~s when creating a digest"
                 ($cannot-make-bundle-digest-openssl-exit-code m))]

        [($cannot-make-bundle-signature? m)
         (format "OpenSSL exited with code ~s when creating a signature"
                 ($cannot-make-bundle-signature-openssl-exit-code m))]

        [($after-delete? m)
         (format "Deleting ~a" ($after-delete-path m))]

        [($reject-user-setting? m)
         (format "Invalid value for ~a: ~a~n  expected: ~a~n  (Note: (void) only applies for `xiden config repl` use)"
                 ($reject-user-setting-name m)
                 ($reject-user-setting-value m)
                 ($reject-user-setting-expected m))]

        [($invalid-launcher-spec? m)
         (format "~s has invalid launcher definition for ~s:~n~a"
                 (format-package-info ($invalid-launcher-spec-info m))
                 ($invalid-launcher-spec-name m)
                 (join-lines (indent-lines ($invalid-launcher-spec-errors m))))]

        [($invalid-workspace-envvar? m)
         (format "Ignoring envvar value for XIDEN_WORKSPACE: ~a~n  falling back to ~a"
                 (getenv "XIDEN_WORKSPACE")
                 (workspace-directory))]

        [($undeclared-racket-version? m)
         (join-lines
          (list (format "~a does not declare a supported Racket version."
                        (format-package-info ($undeclared-racket-version-info m)))
                (format "To install this package anyway, run again with ~a"
                        (setting-short-flag XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))))]

        [($unsupported-racket-version? m)
         (join-lines
          (list (format "~a claims that it does not support this version of Racket (~a)."
                        (format-package-info ($unsupported-racket-version-info m))
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
                                (package-info-racket-versions ($unsupported-racket-version-info m))))))
                (format "To install this package anyway, run again with ~a"
                        (setting-long-flag XIDEN_ALLOW_UNSUPPORTED_RACKET))))]

        [($setup-module-output? m)
         (format "[~a]: ~a"
                 ($setup-module-output-source m)
                 ($setup-module-output-v m))]

        [else (error 'format-xiden-message "Unknown message type: ~s" m)]))


(module+ test
  (require rackunit
           "setting.rkt"
           (submod "package-info.rkt" test))

  (define (capture-bytes p)
    (define buffer (open-output-bytes))
    (p buffer)
    (get-output-bytes buffer #t))

  (define (test-output msg v expected)
    (define buffer (open-output-bytes))

    ; parameterize adds coverage for default value in write-output
    (parameterize ([current-output-port buffer])
      (write-output v))

    (test-true msg
               (if (or (regexp? expected)
                       (pregexp? expected))
                   (regexp-match? expected (get-output-bytes buffer #t))
                   (equal? (get-output-bytes buffer #t) expected))))

  (test-output "By default, program output is human-friendly"
               ($already-installed dummy-package-info)
               #px"already installed")

  (XIDEN_READER_FRIENDLY_OUTPUT #t
    (λ ()
      (test-output "Allow reader-friendly output"
                   ($already-installed dummy-package-info)
                   (capture-bytes
                    (λ (o)
                      (pretty-write #:newline? #t ($already-installed dummy-package-info) o))))

      (XIDEN_FASL_OUTPUT #t
        (λ ()
          (test-case "Allow FASL output"
            (define in
              (open-input-bytes
               (capture-bytes
                (λ (o) (write-output ($already-installed dummy-package-info) o)))))

            (check-equal? (deserialize (fasl->s-exp in))
                          ($already-installed dummy-package-info)))))))

  (test-case "Control verbose output"
    (XIDEN_VERBOSE #f
      (λ ()
        (test-output "Opt out of verbose output"
                     ($verbose ($stop))
                     #"")))

    (parameterize ([(setting-derived-parameter XIDEN_VERBOSE) #t]
                   [(setting-derived-parameter XIDEN_READER_FRIENDLY_OUTPUT) #t])
      (test-output "Opt into verbose output"
                   ($verbose ($stop))
                   #px"\\$stop"))))
