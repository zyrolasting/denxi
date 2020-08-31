#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/generator
         racket/path
         racket/pretty
         racket/sequence
         net/head
         version/utils
         "config.rkt"
         "contract.rkt"
         "encode.rkt"
         "exn.rkt"
         "file.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "mod.rkt"
         "monad.rkt"
         "package-info.rkt"
         "path.rkt"
         "port.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt")

(define-message $undeclared-racket-version (info))
(define-message $unsupported-racket-version (info))
(define-message $package (info))
(define-message $package-installed $package ())


(define (transfer-package-info from-source est-size)
  (define max-size (mibibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB)))
  (define-values (from-pipe to-pipe) (make-pipe max-size))
  (transfer from-source to-pipe
            #:transfer-name "package-info"
            #:max-size max-size
            #:buffer-size (mibibytes->bytes (max (/ (XIDEN_FETCH_PKGDEF_SIZE_MB) 5) 5))
            #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
            #:est-size est-size)
  (close-output-port to-pipe))


(define (make-package-path pkginfo)
  (build-workspace-path "var/xiden/pkg"
                        (make-package-name pkginfo)))


(define (fetch-sourced-package-info source)
  (fetch-named-source source source transfer-package-info))

; (Log (Maybe Derivation))
(define (install-package-from-source source)
  (do path <- (fetch-sourced-package-info source)
      (return (make-package-derivation path))))

(define (make-package-derivation pkginfo)
  (do supported <- (check-racket-support pkginfo)
       (λ (supported?)
         (if supported?
             (make-derivation-module (fold-inputs pkginfo)
                                     (fold-outputs pkginfo))
             (return #f)))))


(define (fold-inputs pkginfo)
  (for/fold ([bound (hash)])
            ([input (package-info-inputs pkginfo)])
    (hash-set bound
              (car input)
              (cadr input))))


(define (fold-outputs pkginfo)
  (for/fold ([bound (hash)])
            ([output (package-info-outputs pkginfo)])
    (hash-set bound
              (car output)
              (cadr output))))


(define (check-racket-support pkginfo)
  (let ([racket-support (check-racket-version-ranges (version) (package-info-racket-versions pkginfo))])
    (case racket-support
      [(supported)
       (return pkginfo)]
      [(unsupported)
       (if (XIDEN_ALLOW_UNSUPPORTED_RACKET)
           (return pkginfo)
           (attach-message #f ($unsupported-racket-version pkginfo)))]
      [(undeclared)
       (if (or (XIDEN_ALLOW_UNSUPPORTED_RACKET)
               (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))
           (return pkginfo)
           (attach-message #f ($undeclared-racket-version pkginfo)))])))


#;(module+ test
  (require racket/runtime-path
           rackunit
           mzlib/etc
           (submod "file.rkt" test)
           "setting.rkt")

  (define me (build-path (this-expression-source-directory) (this-expression-file-name)))

  (test-case "Detect packages that do not declare a supported Racket version"
    (define pkginfo
      (make-package-info #:provider-name "provider"
                         #:package-name "pkg"
                         #:racket-versions null))
    (check-equal? (check-racket-support pkginfo)
                  ($with-messages #f (list ($undeclared-racket-version pkginfo)))))

  (test-case "Detect packages that declare an unsupported Racket version"
    (define pkginfo
      (make-package-info #:provider-name "provider"
                         #:package-name "pkg"
                         #:racket-versions (list "0.0")))

    (check-equal? (check-racket-support pkginfo)
                  ($with-messages #f (list ($unsupported-racket-version pkginfo))))))
