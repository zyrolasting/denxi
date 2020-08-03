#lang racket/base


(provide (all-defined-out)
         (all-from-out racket/format))

(require racket/date
         racket/format
         racket/pretty
         "setting.rkt"
         "string.rkt"
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

(define (format-zcpkg-info-table unsorted-infos)
  (define infos
    (sort unsorted-infos
          #:key (λ (info) (zcpkg-query->string (zcpkg-info->zcpkg-query info)))
          string<?))

  (define (get-cell-printer strs)
    (define min-width (apply max (map string-length strs)))
    (λ args (apply ~a #:min-width min-width args)))

  (define print-provider-name (get-cell-printer (map zcpkg-info-provider-name infos)))
  (define print-package-name  (get-cell-printer (map zcpkg-info-package-name infos)))
  (define print-edition-name  (get-cell-printer (map zcpkg-info-edition-name infos)))
  (define print-revision-num  (get-cell-printer (map (compose ~a zcpkg-info-revision-number) infos)))

  (define row-fmt "~a\t~a\t~a\t~a")
  (format (~a (format row-fmt
                      (print-package-name "Package")
                      (print-provider-name "Provider")
                      (print-edition-name "Edition")
                      (print-revision-num "Revision"))
              "~n~a~n~n")
          (string-join
           (for/list ([info (in-list infos)])
             (format row-fmt
                     (print-package-name (zcpkg-info-package-name info))
                     (print-provider-name (zcpkg-info-provider-name info))
                     (print-edition-name (zcpkg-info-edition-name info))
                     (print-revision-num (zcpkg-info-revision-number info))))
           "\n")))

(define (format-zcpkg-message m)
  (cond [($output? m) (format-zcpkg-message ($output-v m))]

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
         (format "Sources:~n~a~n~n~a~n~a"
                 (join-lines (indent-lines ($review-installation-work-package-sources m)))
                 (format-zcpkg-info-table (map car (hash-values ($review-installation-work-sow m))))
                 (format "To consent to these changes, run again with ~a"
                         (setting->short-flag ZCPKG_CONSENT)))]

        [($review-uninstallation-work? m)
         (format "The following packages will be removed:~n~a~n~n~a"
                 (format-zcpkg-info-table ($review-uninstallation-work-sow m))
                 (format "To consent to these changes, run again with ~a"
                         (setting->short-flag ZCPKG_CONSENT)))]

        [else (~s m)]))
