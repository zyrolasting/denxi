#lang racket/base

(provide (all-defined-out))

(require racket/contract
         racket/exn
         racket/format
         racket/function
         racket/list
         racket/match
         racket/path
         racket/place
         "archiving.rkt"
         "config.rkt"
         "dependency.rkt"
         "download.rkt"
         "file.rkt"
         "installer.rkt"
         "logging.rkt"
         "message.rkt"
         "prompt.rkt"
         "source.rkt"
         "url.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt")

(struct workstate (id pch)
  #:property prop:evt (struct-field-index pch)
  #:property prop:procedure
  (λ (self v)
    (place-channel-put (workstate-pch self) v)
    self))


(define (worker-main pch [state (workstate #f pch)])
  (with-handlers ([exn:break? void]
                  [exn? (λ (e) (state ($on-error (exn->string e))))]
                  [place-message-allowed? state]
                  [(const #t) (λ (v) (state (~s v)))])
    (worker-main pch
                 (handle-message state (sync state)))))

(define (assign-id state id)
  (define next (struct-copy workstate state [id id]))
  (next ($on-idle id)))


(define (install-package state source)
  (define variant (source->variant source))
  (define should-link? (path? variant))
  (define source-path (variant->zcpkg-directory variant))
  (define info (zcpkg-directory->zcpkg-info source-path))
  (define install-path (zcpkg-info->install-path info))
  (define dependencies (zcpkg-info-dependencies info))

  (unless (zcpkg-installed? info)
    (make-zcpkg-install-dir #:link? should-link? source-path install-path)
    (make-zcpkg-workspace-link install-path)
    (make-zcpkg-dependency-links dependencies install-path))

  (state ($on-package-installed info))

  (define unavailable (filter-missing-dependencies dependencies))
  (unless (null? unavailable)
    (state ($on-new-dependencies
            dependencies
            (dependency->string (coerce-dependency info)))))

  (state ($on-idle (workspace-id state))))


(define (uninstall-package state dependency-variant)
  (define target-info (find-info/expect-one dependency-variant))
  (define install-path (zcpkg-info->install-path target-info))
  (for ([maybe-dependent-info (in-installed-info)])
    (when (dependency-match? maybe-dependent-info target-info)
      (uninstall-package state maybe-dependent-info)))
  (delete-directory/files/empty-parents install-path)
  (state ($on-idle (workspace-id state))))

(define (stop state)
  (exit 0))

(define-message-pump (handle-message workstate?)
  assign-id
  stop
  install-package
  uninstall-package)
