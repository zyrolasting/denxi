#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/exn
         racket/path
         racket/place
         racket/sequence
         compiler/cm
         launcher/launcher
         "actor.rkt"
         "archiving.rkt"
         "config.rkt"
         "contract.rkt"
         "download.rkt"
         "file.rkt"
         "format.rkt"
         "message.rkt"
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

    (define/public (create-launcher info
                                    install-path
                                    #:args args
                                    #:name name
                                    #:collects collects
                                    #:aux-path aux-path
                                    #:gracket? gracket?)
      (send-output
       (call/cc
        (λ (return)
          (define accum (make-error-message-accumulator))
          (accum ((listof string?) args) "'args' is not a list of strings")
          (accum (name-string? name) "'name' is not a valid file name")
          (accum ((and/c path-string? (not/c complete-path?)) aux-path)
                 "'aux-path' is not a relative path")

          (define errors (accum))
          (unless (null? errors)
            (return ($invalid-launcher-spec info name errors)))

          (define ctor (if gracket? make-gracket-launcher make-racket-launcher))
          (define dest (build-workspace-path (ZCPKG_LAUNCHER_RELATIVE_PATH) name))
          (make-directory* (path-only dest))
          (ctor args dest null #;(build-aux-from-path (build-path install-path aux-path)))
          (return ($after-write dest))))))

    (define/public (setup-package info exprs
                                  #:install-path
                                  [install-path (zcpkg-info->install-path info)]
                                  #:dependency-infos
                                  [dependency-infos (map find-info (zcpkg-info-dependencies info))])
      (make-zcpkg-dependency-links #:search? #f dependency-infos install-path)
      (make-zcpkg-revision-links info)
      (compile-racket-modules install-path)

      (for ([spec (in-list (zcpkg-info-launchers info))])
        (create-launcher info install-path
                         #:args (hash-ref spec 'args null)
                         #:gracket? (hash-ref spec 'gracket? #f)
                         #:name (hash-ref spec 'name #f)
                         #:collects (hash-ref spec 'collects (hasheq))
                         #:aux-path (hash-ref spec 'aux-path "launcher-aux")))

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

      (setup-package info null
                     #:install-path install-path
                     #:dependency-infos dependency-infos)

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

    (define/public (handle-$setup-package info exprs)
      (setup-package info exprs))

    (define/public (handle-$install-package infos url-or-path)
      (define target (car infos))
      (define dependency-infos (cdr infos))
      (if (zcpkg-installed? target)
          (send-output ($already-installed target))
          (if (directory-exists? url-or-path)
              (install-local-package target dependency-infos url-or-path)
              (install-remote-package target dependency-infos))))))



(define (main pch)
  (send (new zcpkg-worker% [pch pch]) loop))
