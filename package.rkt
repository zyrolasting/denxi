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
         "derivation.rkt"
         "encode.rkt"
         "exn.rkt"
         "file.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "mod.rkt"
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
  (define transfer-output
    (transfer from-source to-pipe
              #:transfer-name "package-info"
              #:max-size max-size
              #:buffer-size (mibibytes->bytes (max (/ (XIDEN_FETCH_PKGDEF_SIZE_MB) 5) 5))
              #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
              #:est-size est-size))
  (close-output-port to-pipe)
  (:merge transfer-output
          (:return (read-package-info from-pipe))))


(define (make-package-path pkginfo)
  (build-workspace-path "var/xiden/pkg"
                        (make-package-name pkginfo)))


(define (install-package-from-source source)
  (:do #:with (fetch-named-source source source transfer-package-info)
       (λ (maybe-path)
         (if maybe-path
             (:return (read-package-info maybe-path))
             (raise (attach-message #f ($no-package-info maybe-path)))))
       make-package))


(define (make-package pkginfo)
  (:do #:with (check-racket-support pkginfo)
       (λ (supported?)
         (if supported?
             (:return #f)))))


(define (fold-inputs pkginfo)
  (for/fold ([bound (hash)])
            ([input (package-info-inputs pkginfo)])
    (hash-set bound
              (input-info-name input)
              (input-info-fetch-info input))))

(define (check-racket-support pkginfo)
  (let ([racket-support (check-racket-version-ranges (version) (package-info-racket-versions pkginfo))])
    (case racket-support
      [(supported)
       (:return pkginfo)]
      [(unsupported)
       (if (XIDEN_ALLOW_UNSUPPORTED_RACKET)
           (:return pkginfo)
           (attach-message #f ($unsupported-racket-version pkginfo)))]
      [(undeclared)
       (if (or (XIDEN_ALLOW_UNSUPPORTED_RACKET)
               (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))
           (:return pkginfo)
           (attach-message #f ($undeclared-racket-version pkginfo)))])))


(module+ test
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
