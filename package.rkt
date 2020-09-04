#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/list
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
         "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "mod.rkt"
         "monad.rkt"
         "package-info.rkt"
         "path.rkt"
         "port.rkt"
         "printer.rkt"
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

(define+provide-message $consent-note ())
(define+provide-message $no-package-info (source))
(define+provide-message $package (info))
(define+provide-message $package-installed $package ())
(define+provide-message $package-not-installed $package ())
(define+provide-message $undeclared-racket-version $package ())
(define+provide-message $unsupported-racket-version $package ())
(define+provide-message $undefined-package-output $package (name))


(define (install-package-from-source source expected-outputs)
  (do pkginfo-path    <- (fetch-package-definition source)
      pkginfo         <- (read-package pkginfo-path)
      checked-pkginfo <- (check-racket-support pkginfo)
      checked-outputs <- (validate-output-request checked-pkginfo expected-outputs)
      build-output    <- (build-package (pick-sandbox-program source pkginfo-path) checked-outputs)
      (return (report-installation-results pkginfo build-output))))


; #lang xiden modules expand such that relative paths are made complete in terms
; of their location. If I load them from the store (which is where pkginfo-path
; points), then the relative paths won't point to anything. Using the original
; source string from the command line (e.g. "../def.rkt") will expand the
; relative paths w.r.t the correct directory.
(define (pick-sandbox-program source pkginfo-path)
  (if (file-exists? source)
      (path->complete-path source)
      pkginfo-path))


(define (report-installation-results pkginfo build-output)
  (logged-attachment build-output
                     (if (eq? build-output SUCCESS)
                         ($package-installed pkginfo)
                         ($package-not-installed pkginfo))))


(define (validate-output-request pkginfo outs [original-outputs outs])
  (cond [(null? outs)
         (logged-unit original-outputs)]

        [(member (car outs)
                 (package-info-outputs pkginfo))
         (validate-output-request pkginfo
                                  (cdr outs)
                                  original-outputs)]

        [else
         (logged-failure ($undefined-package-output pkginfo (car outs)))]))


(define (build-package pkginfo sandbox-program expected-outputs)
  (logged
   (λ (messages)
     (make-addressable-directory
      (build-workspace-path "var/xiden/pkgs")
      (λ (build-dir)
        (define pkgeval (make-package-evaluator sandbox-program build-dir))
        (values SUCCESS
                (cons (for/list ([output (in-list expected-outputs)])
                        (build-package-output pkgeval
                                              pkginfo
                                              output
                                              (build-path build-dir output)))
                      messages)))))))


(define (build-package-output pkgeval pkginfo output-name output-dir)
  (make-directory* output-dir)
  (pkgeval `(cd ,output-dir))
  (pkgeval `(build ,output-name))
  (declare-derivation (package-info-provider-name pkginfo)
                      (package-info-package-name pkginfo)
                      (package-info-edition-name pkginfo)
                      (package-info-revision-number pkginfo)
                      (package-info-revision-number pkginfo)
                      (package-info-revision-names pkginfo)
                      output-dir))



(define (get-package-definition-from-source source)
  (do pkginfo-path <- (fetch-package-definition source)
      pkginfo      <- (read-package-info pkginfo-path)
      (return pkginfo)))


(define (read-package pkginfo-path)
  (logged-unit (read-package-info pkginfo-path)))


(define (fetch-package-definition source)
  (logged
   (λ (m)
     (define fetch-st
       (fetch source
              (list source)
              (λ (in est-size)
                (make-addressable-file
                 source in
                 (min (mibibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB))
                      est-size)))))

     (define-values (result messages) (run-log fetch-st m))

     (values (or (fetch-state-path result) FAILURE)
             messages))))


(define (make-package-evaluator pkginfo-variant build-dir)
  (make-build-sandbox
   (if (file-exists? pkginfo-variant)
       (call-with-input-file pkginfo-variant
         (λ (in) (read-info-module pkginfo-variant in)))
       pkginfo-variant)
   build-dir))



(define (check-racket-support pkginfo)
  (let ([racket-support (check-racket-version-ranges (version) (package-info-racket-versions pkginfo))])
    (case racket-support
      [(supported)
       (logged-unit pkginfo)]
      [(unsupported)
       (if (XIDEN_ALLOW_UNSUPPORTED_RACKET)
           (logged-unit pkginfo)
           (logged-failure ($unsupported-racket-version pkginfo)))]
      [(undeclared)
       (if (or (XIDEN_ALLOW_UNSUPPORTED_RACKET)
               (XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))
           (logged-unit pkginfo)
           (logged-failure ($undeclared-racket-version pkginfo)))])))


(define-message-formatter format-package-message
  [($package-installed name)
   (format "Installed package ~a"
           name)]

  [($undeclared-racket-version info)
   (join-lines
    (list (format "~a does not declare a supported Racket version."
                  info)
          (format "To install this package anyway, run again with ~a"
                  (setting-short-flag XIDEN_ALLOW_UNDECLARED_RACKET_VERSIONS))))]

  [($unsupported-racket-version info)
   (join-lines
    (list (format "~a claims that it does not support this version of Racket (~a)."
                  info
                  (version))
          (format "Supported versions (ranges are inclusive):~n~a~n"
                  (join-lines
                   (map (λ (variant)
                          (format "  ~a"
                                  (if (pair? variant)
                                      (format "~a - ~a"
                                              (or (car variant)
                                                  PRESUMED_MINIMUM_RACKET_VERSION)
                                              (or (cdr variant)
                                                  PRESUMED_MAXIMUM_RACKET_VERSION))
                                      variant))
                          (package-info-racket-versions info)))))
          (format "To install this package anyway, run again with ~a"
                  (setting-long-flag XIDEN_ALLOW_UNSUPPORTED_RACKET))))])


(module+ test
  (require racket/runtime-path
           rackunit
           mzlib/etc
           (submod "file.rkt" test)
           "setting.rkt")

  (define me (build-path (this-expression-source-directory) (this-expression-file-name)))

  (test-case "Reject requests for outputs that a package does not define"
    (define pkginfo
      (make-package-info #:provider-name "provider"
                         #:package-name "pkg"
                         #:outputs '("lib" "doc" "test")))

    (check-equal? (get-log (validate-output-request pkginfo '("blah")))
                  (list ($undefined-package-output pkginfo "blah"))))

  (test-case "Allow requests for outputs that a package does define"
    (define pkginfo
      (make-package-info #:provider-name "provider"
                         #:package-name "pkg"
                         #:outputs '("lib" "doc" "test")))
    (define request '("test" "lib" "doc"))
    (call-with-values (λ () (run-log (validate-output-request pkginfo request)))
                      (λ (v m)
                        (check-equal? v request)
                        (check-pred null? m))))

  (test-case "Detect packages that do not declare a supported Racket version"
    (define pkginfo
      (make-package-info #:provider-name "provider"
                         #:package-name "pkg"
                         #:racket-versions null))
    (check-equal? (get-log (check-racket-support pkginfo))
                  (list ($undeclared-racket-version pkginfo))))

  (test-case "Detect packages that declare an unsupported Racket version"
    (define pkginfo
      (make-package-info #:provider-name "provider"
                         #:package-name "pkg"
                         #:racket-versions (list "0.0")))
    (check-equal? (get-log (check-racket-support pkginfo))
                  (list ($unsupported-racket-version pkginfo)))))
