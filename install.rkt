#lang racket/base

(provide install)

(require idiocket/exn
         idiocket/format
         idiocket/function
         idiocket/match
         idiocket/path
         idiocket/sandbox
         "archiving.rkt"
         "config.rkt"
         "dependency.rkt"
         "download.rkt"
         "file.rkt"
         "installer.rkt"
         "logging.rkt"
         "prompt.rkt"
         "url.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "message.rkt")


(define (install source)
  (define source-directory (get-source-directory source))
  (<< "source ~s resolved to directory ~a~n" source source-directory)
  (define info (getinfo/zcpkg-info source-directory))
  (unless info
    (error 'install
           "No info.rkt file in ~a~n"
           source-directory))
  (if (zcpkg-installed? info)
      (<< "Package already installed: ~a~n"
          (zcpkg-info->install-path info))
      (setup source-directory info)))


; A package source is a string with many possible meanings. Map the
; string to a URL for context. Pick the next step depending on what
; the URL means.
(define (get-source-directory v)
  (cond [(dependency-string? v)
         (download-from-catalog (url->dependency v))]
        [(string? v) (get-source-directory (string->url v))]
        [(find-directory-path v)]
        [else (raise-argument-error 'resolve-source
                                    "A valid package source"
                                    v)]))


(define (setup source-path info)
  (define zcpkg-path (zcpkg-info->install-path info))
  (make-directory* (path-only zcpkg-path))
  (with-handlers ([exn:fail? (λ (e)
                               (delete-directory/files zcpkg-path)
                               (raise e))])
    (copy-directory/files source-path zcpkg-path)
    (make-file-or-directory-link
     (ZCPKG_WORKSPACE)
     (build-path zcpkg-path CONVENTIONAL_WORKSPACE_NAME))
    (define installer (make-installer zcpkg-path info))
    (installer '(set-up!))))


(define (download-from-catalog dep)
  (<< "Download ~a" dep)
  (define artifact-info (download-artifact-info dep))
  (define artifact-path (download-artifact dep))
  (define install-path (zcpkg-info->install-path artifact-info))
  (unpack artifact-path #:to install-path)
  install-path)
