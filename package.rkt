#lang racket/base

(provide (all-defined-out))

(require racket/exn
         racket/format
         racket/function
         racket/generator
         racket/path
         racket/port
         racket/pretty
         racket/runtime-path
         racket/sandbox
         racket/sequence
         version/utils
         "config.rkt"
         "contract.rkt"
         "encode.rkt"
         "file.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "output.rkt"
         "output-info.rkt"
         "package-info.rkt"
         "path.rkt"
         (only-in "printer.rkt" write-output)
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "resolve.rkt"
         "string.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")

(define-runtime-path here "package.rkt")


(define (make-package-path pkginfo)
  (build-workspace-path
   "/var/xiden/store"
   (make-package-name pkginfo)))


(define (install-package! expr)
  (:fold expr
         user-string->package-info
         check-racket-support
         (λ (pkginfo)
           (define path (make-package-path pkginfo))
           (λ () (values pkginfo path)))
         (λ (get-pkginfo+path)
           (define-values (pkginfo path)
             (get-pkginfo+path))
           (if (directory-exists? path)
               (:fail ($already-installed path))
               (run-package! pkginfo path)))))


(define (run-package! pkginfo path)
  (make-directory* path)
  (parameterize ([current-directory path])
    (:fold pkginfo
           (list fetch-inputs!
                 (:lift make-links!)
                 build-outputs!))))


(define (make-links! pkgpath links)
  (for ([(input-name input-path) (in-hash links)])
    (make-file-or-directory-link (find-relative-path pkgpath input-path)
                                 input-name)))


(define (fetch-inputs! inputs)
  (:fold (hash)
         (for/list ([input (in-list inputs)])
           (λ (links)
             (:unit
              (hash-set links
                        (input-info-name input)
                        ($with-output-intermediate (fulfill-input input))))))))


(define (build-outputs! pkginfo)
  (:fold null
         (for/list ([output (package-info-outputs pkginfo)])
           (call-in-sandbox (output-info-builder-name output)
                            (λ (sandbox-values)
                              (cons (sandbox-eval (output-info-builder-expressions output))
                                    sandbox-values))))))


(define (sandbox-eval expressions)
  (parameterize ([current-environment-variables (make-environment-variables)])
    (for/list ([expr (in-list expressions)])
      ((current-eval) expr))))


(define (check-racket-support pkginfo)
  (let ([racket-support
         (check-racket-version-ranges (version)
                                      (package-info-racket-versions pkginfo))])
    (case racket-support
      [(supported)
       (:unit pkginfo)]
      [(unsupported)
       (if (XIDEN_ALLOW_UNSUPPORTED_RACKET)
           (:unit pkginfo)
           (:fail ($unsupported-racket-version pkginfo)))]
      [(undeclared)
       (if (or (XIDEN_ALLOW_UNSUPPORTED_RACKET)
               (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))
           (:unit pkginfo)
           (:unit ($undeclared-racket-version)))])))


(define (enter-package package-path pkg)
  (call-in-sandbox package-path
                   read-eval-print-loop))


(define (call-in-sandbox path proc)
  (parameterize ([sandbox-output (current-output-port)]
                 [sandbox-input (current-input-port)]
                 [sandbox-error-output (current-error-port)]
                 [sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-path-permissions (XIDEN_SANDBOX_PATH_PERMISSIONS)])
    (parameterize ([current-eval (make-module-evaluator #:language 'racket/base path)])
      (proc))))


(module+ test
  (require rackunit)

  (test-case "Detect packages that do not declare a supported Racket version"
    (define output
      (check-racket-support (make-package-info #:provider-name "provider"
                                               #:package-name "pkg"
                                               #:racket-versions null)))
    (check-equal? output
                  ($with-output ($undeclared-racket-version))))

  (test-case "Detect packages that declare an unsupported Racket version"
    (define output
      (check-racket-support
       (make-package-info #:provider-name "provider"
                          #:package-name "pkg"
                          #:racket-versions (list "0.0"))))
    (check-equal? output
     ($with-output ($unsupported-racket-version)))))
