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
         "logging.rkt"
         "message-pump.rkt"
         "message.rkt"
         "prompt.rkt"
         "source.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt")

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


(define (install-package state info package-path)
  (define install-path (zcpkg-info->install-path info))
  (make-directory* (path-only install-path))

  ; These paths are equal if the package was downloaded,
  ; since download-package extracts the archive to the
  ; install location.
  (unless (equal? package-path install-path)
    (make-file-or-directory-link package-path install-path))

  (make-zcpkg-links (zcpkg-info-dependencies info) install-path)
  (state ($on-package-installed info))
  (state ($on-idle (workstate-id state))))


(define (download-package state info catalog-url-string)
  (define install-path (zcpkg-info->install-path info))
  (define artifact-path (download-artifact (string->url catalog-url-string) url))

  (define integrous?
    (or (equal? (make-digest artifact-path)
                (zcpkg-info-integrity info))
        (ZCPKG_TRUST_BAD_DIGEST)))

  (define signature
    (zcpkg-info-signature info))

  (define authenticated?
    (if signature
        (or (verify-signature (zcpkg-info-integrity info)
                              signature
                              (void))
            (ZCPKG_TRUST_BAD_SIGNATURE))
        (ZCPKG_TRUST_UNSIGNED)))

  (unless integrous?
    (state ($on-bad-digest info)))

  (unless authenticated?
    (state ((if signature
                $on-bad-signature
                $on-missing-signature)
            info)))

  (when (and integrous? authenticated?)
    (unpack artifact-path #:to install-path)
    (state ($add-job ($install-package info install-path))))

  (state ($on-idle (workstate-id state))))


(define (uninstall-package state dependency-variant)
  (define target-info (find-exactly-one-info dependency-variant))
  (define install-path (zcpkg-info->install-path target-info))
  (delete-directory/files/empty-parents install-path)
  (state ($on-package-uninstalled target-info))
  (state ($on-idle (workstate-id state))))


(define (resolve-source state source requesting-directory order)
  (define variant (source->variant source requesting-directory))
  (define local? (path? variant))

  (define job
    (if local?
        ($install-package (read-zcpkg-info variant) source)
        (let-values ([(catalog-url info) (download-info variant)])
          ($download-package info (url->string catalog-url) source))))

  (define info
    (if local?
        ($install-package-info job)
        ($download-package-info job)))

  (for ([dependency-source (in-list (zcpkg-info-dependencies info))])
    (state ($add-job
            ($resolve-source dependency-source
                            (if local?
                                source
                                (current-directory))
                            (add1 order)))))

  (state ($backlog-job job))
  (state ($on-idle (workstate-id state))))

(define (stop state)
  (exit 0)
  state)

(define-message-pump (handle-message workstate? default-message-handler)
  assign-id
  stop
  resolve-source
  install-package
  download-package
  uninstall-package)
