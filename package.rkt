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
         "input-info.rkt"
         "integrity.rkt"
         "output.rkt"
         "package-info.rkt"
         "path.rkt"
         (only-in "printer.rkt" write-output)
         "query.rkt"
         "rc.rkt"
         "resolve.rkt"
         "string.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")

(define-runtime-path here "package.rkt")

(define (in-user-requested-package-infos package-defn-input-exprs)
  (sequence-map user-string->package-info
                (in-list package-defn-input-exprs)))

(define (in-package-modules package-infos)
  (sequence-map make-package-module
                package-infos))

(define (save-package! pkginfo pkgmod)
  (call-with-output-file #:exists 'truncate/replace
    (path-replace-extension (make-package-name pkginfo) #".rkt")
    (λ (o) (pretty-write #:newline? #t pkgmod o))))


(define (make-package-module pkginfo)
  `(module package racket/base
     (require (file ,here))
     (module+ main (install!))
     (define def ,pkginfo)
     (define (install!)
       (define distribution-directory (get-inputs pkginfo))
       (unless (directory-exists? distribution-directory)
         (make-directory* distribution-directory)
         (parameterize ([current-directory distribution-directory])
           (for ([input (package-info-inputs def)])
             (define-values (input-path errors) (fulfill-input input))
             (unless (null? errors)
               (for ([e (in-list errors)])
                 (writeln e))
               (exit 1))
             (make-file-or-directory-link input-path (input-info-name input)))
           (for ([output (package-info-outputs def)])
             (call-in-sandbox (output-info-builder-name output)
                              (λ ()
                                (parameterize ([current-environment-variables (make-environment-variables)])
                                  (for ([expr (in-list (output-info-builder-expressions output))])
                                    ((current-eval) expr)))))))))))


(define (check-racket-support pkginfo)
  (let ([racket-support
         (check-racket-version-ranges (version)
                                      (package-info-racket-versions pkginfo))])
    (case racket-support
      [(supported)
       (:use pkginfo)]
      [(unsupported)
       (if (XIDEN_ALLOW_UNSUPPORTED_RACKET)
           (:use pkginfo)
           (:fail ($unsupported-racket-version pkginfo)))]
      [(undeclared)
       (if (or (XIDEN_ALLOW_UNSUPPORTED_RACKET)
               (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))
           (:use pkginfo)
           (send-output ($undeclared-racket-version)))])))


(define (enter-package pkg)
  (call-in-sandbox (package-name pkg)
                   read-eval-print-loop))


(define (setup-package pkg)
  (call-in-sandbox (package-name pkg)
                   (λ ()
                     (for ([expr (in-setup-instructions pkg)])
                       (write-output
                        ($setup-module-output (package-name pkg)
                                              ((current-eval) expr)))))))

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

  #;(test-case "Detect packages that do not declare a supported Racket version"
    (define info (make-xiden-package-info #:provider-name "provider"
                                  #:package-name "pkg"
                                  #:racket-versions null))
    (send worker handle-$install-package info null "")
    (expect-output ($output ($undeclared-racket-version info))))

  #;(test-case "Detect packages that declare an unsupported Racket version"
    (define info (make-xiden-package-info #:provider-name "provider"
                                  #:package-name "pkg"
                                  #:racket-versions (list "0.0")))
    (send worker handle-$install-package info null "")
    (expect-output ($output ($unsupported-racket-version info)))))
