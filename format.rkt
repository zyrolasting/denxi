#lang racket/base


(provide (all-defined-out)
         (all-from-out racket/format))

(require racket/date
         racket/format
         racket/list
         racket/pretty
         "capture.rkt"
         "file.rkt"
         "setting.rkt"
         "string.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-messages.rkt"
         "zcpkg-query.rkt"
         "zcpkg-settings.rkt")

(define (~a* . args)
  (apply ~a (map (λ (s) (~a s "\n")) args)))

(define (indent-lines lines)
  (map (λ (s) (~a "  " s)) lines))

(define (join-lines lines)
  (string-join lines "\n"))

(define (format-zcpkg-info info)
  (zcpkg-query->string (zcpkg-info->zcpkg-query info)))

(define (zcpkg-info->row-data info)
  (list (zcpkg-info-package-name info)
        (zcpkg-info-provider-name info)
        (zcpkg-info-edition-name info)
        (~a (zcpkg-info-revision-number info))))

(define (get-cell-formatter strs)
  (define min-width (apply max (map string-length strs)))
  (λ (#:pad-string [pad-string " "] . args)
    (apply ~a
           #:pad-string pad-string
           #:min-width min-width
           args)))

(define (get-column data i)
  (map (λ (row) (list-ref row i)) data))

(define (get-row-formatter padding col-count data)
  (define row-fmt (string-join (build-list col-count (λ _ "~a")) padding))
  (define col-formatters (map (λ (i) (get-cell-formatter (get-column data i))) (range col-count)))
  (λ args
    (apply format
           row-fmt
           (map (λ (i) ((list-ref col-formatters i) (list-ref args i)))
                (range (length args))))))


(define (format-zcpkg-info-table unsorted-infos)
  (define infos (sort unsorted-infos #:key format-zcpkg-info string<?))
  (define data `(("Package" "Provider" "Edition" "Revision")
                 . ,(map zcpkg-info->row-data infos)))
  (define padding "  ")
  (define col-count (length (car data)))
  (define format-row (get-row-formatter padding col-count data))
  (define first-row (apply format-row (car data)))
  (define hr
    (~a #:pad-string "="
        #:min-width (string-length first-row)))

  (join-lines
   (list
    (apply format-row (car data))
    hr
    (join-lines
     (for/list ([row (in-list (cdr data))])
       (apply format-row row))))))

(define (format-zcpkg-message m)
  (cond [($output? m) (format-zcpkg-message ($output-v m))]

        [($fail? m) ($fail-to-display m)]

        [($already-installed? m)
         (format "~a is already installed at ~a"
                 (zcpkg-query->string (zcpkg-info->zcpkg-query ($already-installed-info m)))
                 (zcpkg-info->install-path ($already-installed-info m)))]

        [($on-compilation-error? m)
         (format "Bytecode compilation error:~n~a"
                 ($on-compilation-error-message m))]

        [($on-bad-digest? m)
         (format (~a "~a failed its integrity check.~n"
                     "While unsafe, you can force installation using ~a.")
                 (zcpkg-query->string (zcpkg-info->zcpkg-query ($on-bad-digest-info m)))
                 (setting->long-flag ZCPKG_TRUST_BAD_DIGEST))]

        [($on-bad-signature? m)
         (format (~a "~a has a signature, but it does not match ~a's public key.~n"
                     "While unsafe, you can trust bad signatures using ~a.")
                 (zcpkg-query->string (zcpkg-info->zcpkg-query ($on-bad-signature-info m)))
                 (zcpkg-info-provider-name ($on-bad-signature-info m))
                 (setting->long-flag ZCPKG_TRUST_BAD_SIGNATURE))]

        [($on-missing-signature? m)
         (format (~a "~a does not have a signature. If you are testing a package, this is expected.~n"
                     "If you got the package from the Internet, then exercise caution!~n"
                     "To trust unsigned packages, use ~a.")
                 (zcpkg-query->string (zcpkg-info->zcpkg-query ($on-missing-signature-info m)))
                 (setting->long-flag ZCPKG_TRUST_UNSIGNED))]

        [($on-unverified-host? m)
         (format (~a "~a does not have a valid certificate.~n"
                     "Connections to this server are not secure.~n"
                     "To trust servers without valid certificates, use ~a.")
                 ($on-unverified-host-host m)
                 (setting->long-flag ZCPKG_TRUST_UNVERIFIED_HOST))]

        [($on-package-installed? m)
         (format "Installed package ~a"
                 (zcpkg-query->string (zcpkg-info->zcpkg-query ($on-package-installed-info m))))]

        [($on-request? m)
         (format "~a ~a ~a client=~a host=~a referer=~a"
                 (parameterize ([date-display-format 'iso-8601])
                   (date->string (seconds->date ($on-request-timestamp m)) #t))
                 (string-upcase (bytes->string/utf-8 ($on-request-method m)))
                 ($on-request-uri m)
                 ($on-request-client-ip m)
                 ($on-request-host-ip m)
                 ($on-request-referer m))]

        [($unrecognized-command? m)
         (format "Unrecognized command: ~s. Run with -h for usage information.~n"
                 ($unrecognized-command-command m))]

        [($review-installation-work? m)
         (define infos (map car (hash-values ($review-installation-work-sow m))))
         (define-values (installed to-install) (partition zcpkg-installed? infos))
         (if (null? to-install)
             "All requested packages are already installed."
             (~a (if (null? installed)
                     ""
                     (format "Installed:~n~a~n"
                             (format-zcpkg-info-table installed)))
                 (format "To install:~n~a~n~n"
                         (format-zcpkg-info-table to-install))
                 (format "To consent to these changes, run again with ~a"
                         (setting->short-flag ZCPKG_CONSENT))))]

        [($review-uninstallation-work? m)
         (format "The following packages will be removed:~n~n~a~n~n~a"
                 (format-zcpkg-info-table ($review-uninstallation-work-sow m))
                 (format "To consent to these changes, run again with ~a"
                         (setting->short-flag ZCPKG_CONSENT)))]

        [($link-command-no-package? m)
         (format "Cannot find a package using ~s."
                 ($link-command-no-package-query-string m))]

        [($setup-command-no-package? m)
         (format "Cannot find a package using ~s."
                 ($setup-command-no-package-query-string m))]

        [($config-command-nonexistant-setting? m)
         (format "There is no setting called ~a.~n"
                 ($config-command-nonexistant-setting-name m))]

        [($chver-command-bad-info? m)
         (format "Cannot change version due to errors in the ~a info:~n~a"
                 ($chver-command-bad-info-name m)
                 ($chver-command-bad-info-errors m))]

        [($package-directory-has-unreadable-info? m)
         (format "Could not read ~a. Double check that ~s points to a package directory."
                 CONVENTIONAL_PACKAGE_INFO_FILE_NAME
                 ($package-directory-has-unreadable-info-package-path m))]

        [($no-package-sources? m)
         "No package sources specified."]

        [($chver-command-updated-info? m)
         (format "~a -> ~a"
                 (format-zcpkg-info ($chver-command-updated-info-old-info m))
                 (format-zcpkg-info ($chver-command-updated-info-new-info m)))]

        [($review-restoration-work? m)
         (join-lines
          `("Please review this planned restoration work carefully."
            "It involves reconfiguring the package manager, which"
            "has security implications. Pay special attention to sandbox permissions!"
            ""
            ,(format "Using capture file: ~a" ($review-restoration-work-capture-file-path m))
            ,(format "Affecting workspace: ~a" ($review-restoration-work-workspace-path m))
            ""
            ,(format "Run the restore command again with ~a to carry out the below instructions."
                     (setting->short-flag ZCPKG_CONSENT))))]

        [($diff-extra-file? m)
         (format "+ ~a" ($diff-extra-file-path m))]

        [($diff-missing-file? m)
         (format "- ~a" ($diff-missing-file-path m))]

        [($diff-different-file? m)
         (format "* ~a" ($diff-different-file-path m))]

        [($diff-same-file? m)
         (format "= ~a" ($diff-same-file-path m))]

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
         (format "Invalid value for ~a: ~s"
                 ($reject-user-setting-name m)
                 ($reject-user-setting-value m))]

        [($new-package-conflict? m)
         (format "Cannot make new package(s). The following files or directories already exist:~n~a"
                 (apply ~a* ($new-package-conflict-existing m)))]

        [($on-server-up? m)
         (format "Server up at ~a. ^C to stop"
                 ($on-server-up-address m))]

        [($on-server-break? m)
         "Shut down server due to user break."]

        [($on-workspace-capture? m)
         (define buf (open-output-string))
         (write-capture ($on-workspace-capture-datum m) buf)
         (get-output-string buf)]

        [($restore-config? m)
         (format "Set ~a to ~s"
                 ($restore-config-name m)
                 ($restore-config-value m))]

        [($restore-delete-file? m)
         (format "Will delete ~a"
                 ($restore-delete-file-path m))]

        [($restore-package? m)
         (format "Will install ~s, unless it exists"
                 ($restore-package-query m))]

        [($invalid-launcher-spec? m)
         (format "~s has invalid launcher definition for ~s:~n~a"
                 (format-zcpkg-info ($invalid-launcher-spec-info m))
                 ($invalid-launcher-spec-name m)
                 (join-lines (indent-lines ($invalid-launcher-spec-errors m))))]

        [($setup-module-output? m)
         (format "[~a]: ~a"
                 ($setup-module-output-source m)
                 ($setup-module-output-v m))]))
