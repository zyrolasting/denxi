#lang racket/base

(provide uninstall)

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
         "string.rkt"
         "url.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt")

(define (uninstall dependency-variant #:include-dependents? [include-dependents? #t])
  ; We need to analyze the impact removal will have on the system.
  (define target-info (find-info/expect-one dependency-variant))
  (define install-path (zcpkg-info->install-path target-info))
  (define installer (make-installer install-path target-info))
  (installer '(tear-down!))
  (delete-directory/files/empty-parents install-path)
  (when include-dependents?
    (for ([maybe-dependent-info (in-installed-info)])
      (when (dependency-match? maybe-dependent-info target-info)
        (uninstall maybe-dependent-info)))))
