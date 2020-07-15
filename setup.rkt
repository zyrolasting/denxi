#lang racket/base

; Define a configurable sandbox evaluator that allows a user to pass
; control to a package's installer.

(provide enter-setup-module)

(require racket/exn
         racket/format
         racket/path
         racket/function
         racket/sandbox
         "workspace.rkt"
         "config.rkt"
         "logging.rkt"
         "dependency.rkt"
         "url.rkt"
         "zcpkg-info.rkt")


(define (enter-setup-module zcpkg-path info)
  (define name (dependency->string (zcpkg-info->dependency info)))
  (define setup-module-path (get-setup-module-path zcpkg-path name info))
  (when setup-module-path
    (parameterize ([sandbox-output (current-output-port)]
                   [sandbox-input (current-input-port)]
                   [sandbox-error-output (current-error-port)])
      (parameterize ([current-eval (make-module-evaluator #:language 'racket/base setup-module-path)])
        (read-eval-print-loop)))
    void))


(define (get-setup-module-path zcpkg-path name info)
  (define setup-module-path (zcpkg-info-setup-module info))
  (define path-to-verify (simplify-path (build-path zcpkg-path setup-module-path)))
  (and setup-module-path
       (if (path-prefix? path-to-verify zcpkg-path)
           path-to-verify
           (error 'setup
                  "~a's setup-module path reaches outside of install directory."
                  name))))

(define (path-prefix? to-check prefix-pathy)
  (define maybe-prefixed (explode-path (simplify-path (path->complete-path to-check))))
  (define pref (explode-path (simplify-path (path->complete-path prefix-pathy))))

  (and (<= (length pref)
           (length maybe-prefixed))
       (for/and ([(el index) (in-indexed pref)])
         (equal? (list-ref maybe-prefixed index)
                 el))))
