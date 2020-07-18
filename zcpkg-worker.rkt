#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/exn
         racket/path
         racket/place
         "actor.rkt"
         "archiving.rkt"
         "config.rkt"
         "download.rkt"
         "file.rkt"
         "message.rkt"
         "sentry.rkt"
         "source.rkt"
         "string.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt"
         "zcpkg-team.rkt")

; Output messages
(define-message $start (workspace-dir))
(define-message $before-making-orphans (dependents dependency))
(define-message $resolve-source (source requesting-directory))
(define-message $on-bad-digest (info))
(define-message $on-bad-signature (info))
(define-message $on-missing-signature (info))
(define-message $on-unverified-host (host))
(define-message $on-package-installed (info))
(define-message $on-package-uninstalled (info))
(define-message $install-package (info url-or-path))
(define-message $uninstall-package (dependency-string))

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
      (pump (sync pch))
      (loop))

    (define/public (handle-$start workspace-dir)
      (workspace-directory workspace-dir)
      (load-zcpkg-settings!))

    (define/public (handle-$stop state)
      (exit 0))

    (define/public (install-local-package info package-path)
      (define install-path (zcpkg-info->install-path info))

      (unless (equal? package-path install-path)
        (make-directory* (path-only install-path))
        (make-file-or-directory-link package-path install-path))

      (make-zcpkg-links (zcpkg-info-dependencies info) install-path)
      (send-output ($on-package-installed info)))


    (define/public (install-remote-package info catalog-url-string)
      (define install-path   (zcpkg-info->install-path info))
      (define artifact-path  (download-artifact (string->url catalog-url-string) url))
      (define integrous?     (integrous-artifact? artifact-path info))
      (define authenticated? (authenticated-provider? info (void)))

      (unless integrous?
        (send-output ($on-bad-digest info)))

      (unless authenticated?
        (send-output ((if (zcpkg-info-signature info)
                              $on-bad-signature
                              $on-missing-signature)
                          info)))

      (when (and integrous? authenticated?)
        (make-directory* (path-only install-path))
        (unpack artifact-path #:to install-path)
        (install-local-package info install-path)))


    (define/public (handle-$install-package info url-or-path)
      (if (directory-exists? url-or-path)
          (install-local-package info url-or-path)
          (install-remote-package info url-or-path)))


    (define/public (handle-$uninstall-package dependency-variant)
      (define target-info (find-exactly-one-info dependency-variant))
      (define install-path (zcpkg-info->install-path target-info))
      (delete-directory/files/empty-parents install-path)
      (send-output ($on-package-uninstalled target-info)))


    (define/public (handle-$resolve-source source requesting-directory)
      (define variant (source->variant source requesting-directory))
      (define local? (path? variant))

      (define job
        (if local?
            ($install-package (read-zcpkg-info variant) variant)
            (let-values ([(catalog-url info) (download-info variant)])
              ($install-package info (url->string catalog-url)))))

      (for ([dependency-source (in-list (zcpkg-info-dependencies ($install-package-info job)))])
        (handle-$resolve-source dependency-source
                                (if local? variant (current-directory))))

      (send-output job))))


(define (main pch)
  (with-handlers ([exn:break? void]
                  [(const #t)
                   (λ (e)
                     (place-channel-put pch
                                        ($fail (if (exn? e) (exn->string e) (~s e)))))])
    (send (new zcpkg-worker% [pch pch]) loop)))
