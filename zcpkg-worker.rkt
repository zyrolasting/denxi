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
         "team.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt")

; Output messages
(define-message $start (workspace-dir))
(define-message $before-making-orphans (dependents dependency))
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
      (with-handlers ([(const #t) (λ (e) (fail e))])
        (let again ()
          (pump (sync pch))
          (again))))

    (define/public (fail e)
      (send-up ($fail (if (exn? e) (exn->string e) (~s e))))
      (exit 1))

    (define/public (handle-$start workspace-dir)
      (workspace-directory workspace-dir)
      (load-zcpkg-settings!))

    (define/public (handle-$stop)
      (exit 0))

    (define/public (handle-$sentinel)
      (send-up ($sentinel)))

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
      (send-output ($on-package-uninstalled target-info)))))



(define (main pch)
  (send (new zcpkg-worker% [pch pch]) loop))
