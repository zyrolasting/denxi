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
         "sandbox.rkt"
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

(define+provide-message $consent-note ())
(define+provide-message $no-package-info (source))

(define (fetch-package-definition source)
  (fetch (list source)
         (λ (in est-size)
           (make-addressable-file source in est-size))))

(define (install-package-from-source source expected-outputs)
  (define pkginfo-path (fetch-package-definition source))
  (define pkginfo (read-package-info pkginfo-path))
  (define actual-outputs (package-info-outputs pkginfo))
  (for ([expected-output (in-list expected-outputs)])
    (unless (member expected-output actual-outputs)
      (error "Output not defined by package")))
  (define seval (make-build-sandbox pkginfo-path))
  (call-with-build-directory
   (build-workspace-path "var/xiden/pkgs")
   (for ([output (in-list expected-outputs)])
     (seval `(build ,output)))))

(define (call-with-build-directory containing-dir proc)
  (call-with-temporary-directory
   #:cd? #t #:base containing-dir
   (λ (path)
     (proc path)
     (define dest
       (build-path (path-only path)
                   (encoded-file-name
                    (make-directory-content-digest path))))

     (with-handlers ([exn:fail?
                      (λ (e)
                        (copy-directory/files path dest #:preserve-links? #t)
                        (delete-directory/files path))])
       (rename-file-or-directory path dest #:exists-ok? #t)))))


(define (make-directory-content-digest path)
  (for/fold ([dig #""])
            ([subpath (in-directory path)]
             #:when (file-exists? subpath))
    (call-with-input-file subpath
      (λ (in) (make-digest (input-port-append (open-input-bytes dig) in))))))

(define (encoded-file-name variant)
  (define encoded (encode 'base32 variant))
  (define as-string
    (if (bytes? encoded)
        (bytes->string/utf-8 encoded)
        encoded))

  (substring as-string 0
             (min (string-length as-string) 32)))



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
