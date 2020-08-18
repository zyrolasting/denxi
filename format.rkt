#lang racket/base


(provide (all-defined-out)
         (all-from-out racket/format))

(require racket/date
         racket/exn
         racket/format
         racket/list
         racket/match
         racket/pretty
         "file.rkt"
         "package-info.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "setting.rkt"
         "string.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")

(define (~a* . args)
  (apply ~a (map (λ (s) (~a s "\n")) args)))

(define (indent-lines lines)
  (map (λ (s) (~a "  " s)) lines))

(define (join-lines lines)
  (string-join lines "\n"))

(define (format-package-info info)
  (package-info-package-name info))

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

        [($on-module-compiled? m)
         (format "Compiled: ~a"
                 ($on-module-compiled-module-path m))]

        [($on-compilation-error? m)
         (format "Bytecode compilation error in: ~a~n~a"
                 ($on-compilation-error-module-path m)
                 ($on-compilation-error-message m))]

        [($on-bad-digest? m)
         (format (~a "~a failed its integrity check.~n"
                     "While unsafe, you can force installation using ~a.")
                 (format-package-info ($on-bad-digest-info m))
                 (setting-long-flag XIDEN_TRUST_BAD_DIGEST))]

        [($mod-load-failure? m)
         (format (~a "Could not load plugin module ~a. Using default implementations.~n"
                     "Load error: ~a")
                 ($mod-load-failure-path m)
                 ($mod-load-failure-original-error-string m))]

        [($on-bad-signature? m)
         (format (~a "~a has a signature, but it does not match ~a's public key.~n"
                     "While unsafe, you can trust bad signatures using ~a.")
                 (format-package-info ($on-bad-signature-info m))
                 (package-info-provider-name ($on-bad-signature-info m))
                 (setting-long-flag XIDEN_TRUST_BAD_SIGNATURE))]

        [($on-missing-signature? m)
         (format (~a "~a does not have a signature. If you are testing a package, this is expected.~n"
                     "If you got the package from the Internet, then exercise caution!~n"
                     "To trust unsigned packages, use ~a.")
                 (format-package-info ($on-missing-signature-info m))
                 (setting-long-flag XIDEN_TRUST_UNSIGNED))]

        [($on-unverified-host? m)
         (format (~a "~a does not have a valid certificate.~n"
                     "Connections to this server are not secure.~n"
                     "To trust servers without valid certificates, use ~a.")
                 ($on-unverified-host-host m)
                 (setting-long-flag XIDEN_TRUST_UNVERIFIED_HOST))]

        [($on-package-installed? m)
         (format "Installed package ~a"
                 (format-package-info ($on-package-installed-info m)))]

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

        [($package-directory-has-unreadable-info? m)
         (format "Could not read ~a. Double check that ~s points to a package directory."
                 CONVENTIONAL_PACKAGE_INFO_FILE_NAME
                 ($package-directory-has-unreadable-info-package-path m))]

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

        [($on-server-up? m)
         (format "Server up at ~a. ^C to stop"
                 ($on-server-up-address m))]

        [($on-server-break? m)
         "Shut down server due to user break."]

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
