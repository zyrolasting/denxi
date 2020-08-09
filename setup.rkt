#lang racket/base

; Define a configurable sandbox evaluator that allows a user to pass
; control to a package's installer.

(provide enter-setup-module
         load-in-setup-module)

(require racket/exn
         racket/format
         racket/path
         racket/function
         racket/sandbox
         "workspace.rkt"
         "config.rkt"
         "zcpkg-query.rkt"
         "url.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt")


(define (enter-setup-module info)
  (call-in-setup-module info read-eval-print-loop))

(define (load-in-setup-module info exprs)
  (call-in-setup-module info
                        (λ ()
                          (for/list ([expr exprs])
                            ((current-eval) expr)))
                        (λ () null)))

(define (call-in-setup-module info proc [fail-thunk void])
  (define setup-module-path (get-setup-module-path info))
  (if setup-module-path
      (parameterize ([sandbox-output (current-output-port)]
                     [sandbox-input (current-input-port)]
                     [sandbox-error-output (current-error-port)]
                     [sandbox-memory-limit (ZCPKG_SANDBOX_MEMORY_LIMIT_MB)]
                     [sandbox-eval-limits (list (ZCPKG_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                                (ZCPKG_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                     [sandbox-path-permissions (ZCPKG_SANDBOX_PATH_PERMISSIONS)])
        (parameterize ([current-eval (make-module-evaluator #:language 'racket/base setup-module-path)])
          (proc)))
      (fail-thunk)))



(define (get-setup-module-path info)
  (define name (zcpkg-query->string (zcpkg-info->zcpkg-query info)))
  (define zcpkg-path (zcpkg-info->install-path info))
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
