#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/exn
         racket/path
         racket/place
         racket/sequence
         compiler/cm
         "actor.rkt"
         "archiving.rkt"
         "config.rkt"
         "contract.rkt"
         "download.rkt"
         "file.rkt"
         "format.rkt"
         "message.rkt"
         "racket-version.rkt"
         "resolve.rkt"
         "sentry.rkt"
         "setup.rkt"
         "string.rkt"
         "team.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-messages.rkt"
         "zcpkg-query.rkt"
         "zcpkg-settings.rkt")

(define zcpkg-worker%
  (class actor%
    (super-new)
    (inherit pump)
    (init-field pch)

    (define/public (send-up v)
      (place-channel-put pch v))

    (define/public (send-output v)
      (send-up ($output v)))

    (define/public (loop)
      (with-handlers ([(const #t) (λ (e) (fail e))])
        (let again ()
          (pump (sync pch))
          (again))))

    (define/public (fail e)
      (send-up ($fail (if (exn? e) (exn->string e) (~s e))))
      (exit 1))

    (define/public (handle-$start workspace-dir dump)
      (workspace-directory workspace-dir)
      (configure-zcpkg! dump))

    (define/public (handle-$stop)
      (exit 0))

    (define/public (handle-$sentinel)
      (send-up ($sentinel)))

    (define/public (compile-racket-modules install-path)
      (for ([module-path (in-racket-modules install-path)])
        (with-handlers
          ([exn?
            (λ (e) (send-output
                    ($on-compilation-error
                     (path->string module-path)
                     (exn->string e))))])
          (managed-compile-zo module-path))))

    (define/public (setup-package info dependency-infos exprs)
      (define install-path (zcpkg-info->install-path info))
      (compile-racket-modules install-path)
      (make-zcpkg-dependency-links #:search? #f dependency-infos install-path)
      (make-zcpkg-revision-links info)
      (create-launchers info)
      (for ([output (load-in-setup-module info (in-list exprs))])
        (send-output ($setup-module-output
                      (zcpkg-query->string (zcpkg-info->zcpkg-query info))
                      (~s output)))))

    (define/public (install-local-package info dependency-infos package-path)
      (define install-path (zcpkg-info->install-path info))

      (unless (equal? package-path install-path)
        (make-directory* (path-only install-path))
        (if (ZCPKG_LINK)
            (make-file-or-directory-link package-path install-path)
            (copy-directory/files package-path install-path)))

      (send-output ($on-package-installed info)))


    (define/public (install-remote-package info dependency-infos)
      (define install-path   (zcpkg-info->install-path info))
      (define query          (coerce-zcpkg-query info))
      (define artifact-path  (download-artifact query))
      (define public-key     (download-public-key (zcpkg-query-provider-name query)))
      (define integrous?     (integrous-artifact? artifact-path info))
      (define authenticated? (authenticated-provider? info public-key))

      (unless integrous?
        (send-output ($on-bad-digest info)))

      (unless authenticated?
        (send-output ((if (zcpkg-info-signature info)
                              $on-bad-signature
                              $on-missing-signature)
                          info)))

      (when (and integrous? authenticated?)
        (make-directory* (path-only install-path))
        (unpack artifact-path install-path)
        (install-local-package info dependency-infos install-path)))

    (define/public (handle-$setup-package info dependency-infos exprs)
      (setup-package info dependency-infos exprs))

    (define/public (handle-$install-package info dependency-infos url-or-path)
      (define (do-install)
        (if (directory-exists? url-or-path)
            (install-local-package info dependency-infos url-or-path)
            (install-remote-package info dependency-infos)))

      (if (zcpkg-installed? info)
          (send-output ($already-installed info))
          (case (check-racket-version-ranges (version) (zcpkg-info-racket-versions info))
            [(supported) (do-install)]
            [(unsupported)
             (if (ZCPKG_ALLOW_UNSUPPORTED_RACKET)
                 (do-install)
                 (send-output ($unsupported-racket-version info)))]
            [(undeclared)
             (if (or (ZCPKG_ALLOW_UNSUPPORTED_RACKET)
                     (ZCPKG_ALLOW_UNDECLARED_RACKET_VERSIONS))
                 (do-install)
                 (send-output ($undeclared-racket-version info)))])))))



(define (main pch)
  (send (new zcpkg-worker% [pch pch]) loop))
