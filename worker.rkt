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
         racket/set
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

(define (find-dependencies source [found (set)])
  (define variant (source->variant source))
  (define info (variant->zcpkg-info variant))
  (define deps (zcpkg-info-dependencies info))
  (define accum
   (set-union found
              (apply set deps)))
  (for/fold ([wip accum])
            ([dep (in-list deps)])
    (find-dependencies dep wip)))


(define (install-package state source)
  (define variant (source->variant source))
  (define info (variant->zcpkg-info variant))
  (define install-path (zcpkg-info->install-path info))
  (define dependencies (zcpkg-info-dependencies info))

  (unless (zcpkg-installed? info)
    (if (path? variant)
        (begin (make-directory* (path-only install-path))
               (make-file-or-directory-link variant install-path))
        (unpack (download-artifact source)
                #:to install-path))
    (make-zcpkg-dependency-links dependencies install-path))

  (state ($on-package-installed info))

  (define unavailable (filter-missing-dependencies dependencies))
  (unless (null? unavailable)
    (state ($on-new-dependencies
            dependencies
            (dependency->string (coerce-dependency info)))))

  (state ($on-idle (workstate-id state))))


(define (uninstall-package state dependency-variant)
  (define target-info (find-exactly-one-info dependency-variant))
  (define install-path (zcpkg-info->install-path target-info))
  (for ([maybe-dependent-info (in-installed-info)])
    (when (dependency-match? maybe-dependent-info target-info)
      (uninstall-package state maybe-dependent-info)))
  (delete-directory/files/empty-parents install-path)
  (state ($on-idle (workstate-id state))))

(define (stop state)
  (exit 0)
  state)

(define-message-pump (handle-message workstate?)
  assign-id
  stop
  install-package
  uninstall-package)
