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
         "download.rkt"
         "file.rkt"
         "format.rkt"
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
(define-message $already-installed (info))
(define-message $on-compilation-error (message))
(define-message $on-bad-digest (info))
(define-message $on-bad-signature (info))
(define-message $on-missing-signature (info))
(define-message $on-unverified-host (host))
(define-message $on-package-installed (info))
(define-message $on-package-uninstalled (info))
(define-message $install-package (infos url-or-path))
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

    (define/public (compile-racket-modules install-path)
      (for ([module-path
             (sequence-filter (λ (p)
                                (and (not (link-exists? p))
                                     (not (member (path->string (file-name-from-path p))
                                                  (list CONVENTIONAL_WORKSPACE_NAME
                                                        CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME)))
                                     (file-exists? p)
                                     (member (path-get-extension p)
                                             '(#".rkt" #".ss" #".scrbl"))))
                              (in-acyclic-directory install-path))])
        (with-handlers ([exn? (λ (e) (send-output ($on-compilation-error (exn->string e))))])
          (managed-compile-zo module-path))))


    (define/public (install-local-package info dependency-infos package-path)
      (define install-path (zcpkg-info->install-path info))

      (unless (equal? package-path install-path)
        (make-directory* (path-only install-path))
        (make-file-or-directory-link package-path install-path))

      (make-zcpkg-links #:search? #f dependency-infos install-path)
      (compile-racket-modules install-path)
      (send-output ($on-package-installed info)))


    (define/public (install-remote-package info dependency-infos catalog-url-string)
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
        (install-local-package info dependency-infos install-path)))


    (define/public (handle-$install-package infos url-or-path)
      (define target (car infos))
      (define dependency-infos (cdr infos))
      (if (zcpkg-installed? target)
          (send-output ($already-installed target))
          (if (directory-exists? url-or-path)
              (install-local-package target dependency-infos url-or-path)
              (install-remote-package target dependency-infos url-or-path))))


    (define/public (handle-$uninstall-package dependency-variant)
      (define target-info (find-exactly-one-info dependency-variant))
      (define install-path (zcpkg-info->install-path target-info))
      (delete-directory/files/empty-parents install-path)
      (send-output ($on-package-uninstalled target-info)))))



(define (main pch)
  (send (new zcpkg-worker% [pch pch]) loop))
