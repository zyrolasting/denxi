#lang racket/base

; Define operations for package setup, assuming all package files are
; present on the system.

(provide enter-setup-module
         load-in-setup-module
         make-collects-expression)

(require racket/exn
         racket/format
         racket/path
         racket/function
         racket/sandbox
         "config.rkt"
         "file.rkt"
         "url.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-query.rkt"
         "zcpkg-settings.rkt")

; Generate a Racket expression that would tell the module resolver to
; look for particular packages when given specific collection names.
(define (make-collects-expression collects)
  `(let ([old (current-library-collection-links)])
     (current-library-collection-links
      (cons ,(for/hash ([(sym query) (in-hash collects)])
               (values sym (list (path->string (zcpkg-info->install-path (find-latest-info query))))))
            old))))

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
  (and setup-module-path
       (let ([path-to-verify (simplify-path (build-path zcpkg-path setup-module-path))])
         (if (path-prefix? path-to-verify zcpkg-path)
             path-to-verify
             (error 'setup
                    "~a's setup-module path reaches outside of install directory."
                    name)))))


(define (path-prefix? to-check prefix-pathy)
  (define maybe-prefixed (explode-path (simplify-path (path->complete-path to-check))))
  (define pref (explode-path (simplify-path (path->complete-path prefix-pathy))))

  (and (<= (length pref)
           (length maybe-prefixed))
       (for/and ([(el index) (in-indexed pref)])
         (equal? (list-ref maybe-prefixed index)
                 el))))
