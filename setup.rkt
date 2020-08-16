#lang racket/base

; Define operations for package setup, assuming all package files are
; present on the system.

(provide (all-defined-out))

(require racket/exn
         racket/format
         racket/path
         racket/function
         racket/sandbox
         racket/sequence
         launcher/launcher
         "config.rkt"
         "contract.rkt"
         "file.rkt"
         "path.rkt"
         "string.rkt"
         "url.rkt"
         "verify.rkt"
         "workspace.rkt"
         "zcpkg-info.rkt"
         "zcpkg-messages.rkt"
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

(define (create-launcher info install-path spec)
  (call/cc
   (λ (return)
     (define with-defaults (add-launcher-spec-defaults spec))
     (define (get k) (hash-ref with-defaults k))

     (define args (get 'args))
     (define name (get 'name))
     (define aux-path (get 'aux-path))
     (define gracket? (get 'gracket?))
     (define collects (get 'collects))

     (define accum (make-error-message-accumulator))
     (accum ((listof string?) args) "'args' is not a list of strings")
     (accum (name-string? name) "'name' is not a valid file name")
     (accum ((and/c path-string? (not/c complete-path?)) aux-path)
            "'aux-path' is not a relative path")

     (if (hash? collects)
         (for ([(maybe-sym maybe-query) (in-hash collects)])
           (accum (symbol? maybe-sym)
                  (format "~s is not a valid collection name in 'collects'."
                          maybe-sym))
           (accum (zcpkg-query-string? maybe-query)
                  (format "~s is not a valid package query in 'collects'."
                          maybe-query)))
         (accum #f "'collects' is not a hash table"))

     (define errors (accum))
     (unless (null? errors)
       (return ($invalid-launcher-spec info name errors)))

     (define args-with-collections
       (if (> (hash-count collects) 0)
           (append `("-e" ,(~s (make-collects-expression collects)))
                   args)
           args))

     (define ctor (if gracket? make-gracket-launcher make-racket-launcher))
     (define dest (build-workspace-path (ZCPKG_LAUNCHER_RELATIVE_PATH) name))
     (make-directory* (path-only dest))
     (ctor args-with-collections dest null #;(build-aux-from-path (build-path install-path aux-path)))
     (return ($after-write dest)))))


(define (create-launchers info)
  (define install-path (zcpkg-info->install-path info))
  (for/list ([spec (in-list (zcpkg-info-launchers info))])
    (create-launcher info install-path (add-launcher-spec-defaults spec))))

(define (delete-launcher spec)
  (define launcher-path
    (build-workspace-path (ZCPKG_LAUNCHER_RELATIVE_PATH)
                          (hash-ref spec 'name)))
  (delete-file* launcher-path))

(define (delete-launchers info)
  (for/fold ([deleted null])
            ([spec (in-list (zcpkg-info-launchers info))])
    (define maybe-launcher-path (delete-launcher spec))
    (if maybe-launcher-path
        (cons maybe-launcher-path deleted)
        deleted)))

(define (make-zcpkg-links info dependency-infos)
  (sequence-map $after-write
                (sequence-append
                 (in-list (make-zcpkg-dependency-links
                           #:search? #f
                           dependency-infos
                           (zcpkg-info->install-path info)))

                 (in-list (make-zcpkg-revision-links info)))))

(define (make-zcpkg-dependency-links #:search? search? dependencies [where (current-directory)])
  (if (null? dependencies)
      null
      (for/list ([variant (in-list dependencies)])
        (define dependency-info (if search? (find-exactly-one-info variant) variant))
        (make-link/clobber (zcpkg-info->install-path dependency-info)
                           (build-dependency-path where dependency-info)))))


(define (make-zcpkg-revision-links info
                                   #:newest? [newest? #f]
                                   #:target [target (zcpkg-info->install-path info)])
  (parameterize ([current-directory (or (path-only target) (current-directory))])
    (define user-specified
      (for/list ([revision-name (in-list (zcpkg-info-revision-names info))])
        (path->complete-path (make-link/clobber target revision-name))))
    (if newest?
        (cons (path->complete-path (make-link/clobber target CONVENTIONAL_NEWEST_REVISION_NAME))
              user-specified)
        user-specified)))


(define (delete-zcpkg-revision-links info)
  (define edition-path (path-only (zcpkg-info->install-path info)))
  (for/fold ([deleted null])
            ([revision-name (in-list (cons "newest" (zcpkg-info-revision-names info)))])
    (define target (build-path edition-path revision-name))
    (if (delete-file* target)
        (cons target deleted)
        deleted)))


(define (enter-setup-module info)
  (call-in-setup-module info read-eval-print-loop))

(define (load-in-setup-module info exprs)
  (define pkg-name (zcpkg-query->string (zcpkg-info->zcpkg-query info)))
  (call-in-setup-module info
                        (λ ()
                          (for/list ([expr (in-list exprs)])
                            ($setup-module-output pkg-name (~s ((current-eval) expr)))))
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
       (let ([path-to-verify (build-strictly-deeper-path zcpkg-path setup-module-path)])
         (or path-to-verify
             (error 'setup
                    "~a's setup-module path reaches outside of install directory."
                    name)))))
