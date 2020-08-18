#lang racket/base

(provide (all-defined-out))

(require racket/exn
         racket/format
         racket/function
         racket/generator
         racket/path
         racket/port
         racket/pretty
         racket/sandbox
         racket/sequence
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

(struct package (name definition))

(define (in-user-requested-package-infos package-defn-input-exprs)
  (sequence-map input-expr->package-info
                (in-list package-defn-input-exprs)))

(define (in-package-modules package-infos)
  (sequence-map (compose get-package-module make-package)
                package-infos))

(define (make-package pkginfo)
  (package (make-package-name pkginfo)
           pkginfo))

(define (save-package! pkg)
  (call-with-output-file #:exists 'truncate/replace
    (path-replace-extension (package-name pkg) #".rkt")
    (λ (o) (write-package pkg o))))

(define (write-package pkg o)
  (pretty-write #:newline? #t (get-package-module pkg) o))

(define (get-package-module pkg)
  (define distribution-name (package-name pkg))
  (define setup-module (package-info-setup-module pkg))
  `(module package racket/base
     (require xiden version/utils)
     ,setup-module
     (define (install!) . ,(get-installation-instructions pkg))
     (module+ main (install!) (setup!))))

(define (get-installation-instructions pkg)
  (define distribution-name (package-name pkg))
  (in-generator
   (yield `(if (directory-exists? ,distribution-name)
               (write-output ($already-installed))
               (parameterize ([current-directory distribution-name])
                 (make-directory* distribution-name)
                 (void))))))

(define (get-racket-support-check versions)
  `(let ([racket-support (check-racket-version-ranges (version) ,versions)])
     (case racket-support
       [(supported) (do-install)]
       [(unsupported)
        (unless ,(XIDEN_ALLOW_UNSUPPORTED_RACKET)
          (write-output ($unsupported-racket-version info)))]
       [(undeclared)
        (unless ,(or (XIDEN_ALLOW_UNSUPPORTED_RACKET)
                (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))
          (send-output ($undeclared-racket-version)))])))

(define (make-package-name pkginfo)
  (format "~a-~a"
          (encode 'base32 (make-package-info-digest pkginfo))
          (package-info-package-name pkginfo)))

(define (make-package-info-digest pkginfo)
  (make-digest 'sha384
   (apply input-port-append
          (map open-input-bytes
               (sequence->list
                (in-generator
                 (yield (string->bytes/utf-8 (package-info-package-name pkginfo)))
                 (for ([input (in-list (package-info-inputs pkginfo))])
                   (yield (integrity-info-digest
                           (input-info-integrity input))))))))))


(define (enter-package pkg)
  (call-in-sandbox (package-name pkg)
                   read-eval-print-loop))

(define in-setup-instructions void)

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
