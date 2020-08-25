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
         net/head
         version/utils
         "config.rkt"
         "contract.rkt"
         "encode.rkt"
         "exn.rkt"
         "file.rkt"
         "integrity.rkt"
         "mod.rkt"
         "output.rkt"
         "package-info.rkt"
         "path.rkt"
         "port.rkt"
         (only-in "printer.rkt" write-output)
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "resolve.rkt"
         "signature.rkt"
         "string.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")


(define-exn exn:fail:xiden:source exn:fail:xiden (input-name source))
(define-exn exn:fail:xiden:source:no-content exn:fail:xiden:source ())
(define-exn exn:fail:xiden:source:digest-mismatch exn:fail:xiden:source ())
(define-exn exn:fail:xiden:source:signature-mismatch exn:fail:xiden:source ())


(define (make-package-path pkginfo)
  (build-workspace-path (XIDEN_INSTALL_RELATIVE_PATH)
                        (make-package-name pkginfo)))


(define (install-package! expr)
  (:fold expr
         (list user-string->package-info
               check-racket-support
               (λ (pkginfo)
                 (define path (make-package-path pkginfo))
                 (if (directory-exists? path)
                     (:fail ($already-installed path))
                     (run-package! pkginfo path))))))


(define (run-package! pkginfo path)
  (make-directory* path)
  (parameterize ([current-directory path])
    (:fold pkginfo
           (list fetch-inputs!
                 (λ (links) (:unit (make-links! path links)))
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
           (λ (sandbox-values)
             (define seval (make-sandbox (output-info-builder-name output)))
             (cons (for/list ([expr (in-list (output-info-builder-expressions output))])
                     (seval expr))
                   sandbox-values)))))


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


(define (make-sandbox path)
  (parameterize ([sandbox-output (current-output-port)]
                 [sandbox-input (current-input-port)]
                 [sandbox-error-output (current-error-port)]
                 [sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-path-permissions (XIDEN_SANDBOX_PATH_PERMISSIONS)]
                 [sandbox-make-environment-variables make-environment-variables])
    (make-module-evaluator #:language 'racket/base path)))



(define (fetch-source/filesystem source make-input-file build-derivation make-link)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (and (file-exists? source)
         (make-link (make-input-file (open-input-file source)
                                     (+ (* 20 1024) ; for Mac OS resource forks
                                        (file-size source)))))))


(define (fetch-source/http source make-input-file build-derivation make-link)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define in (head-impure-port (string->url source)))
    (define headers (extract-all-fields (port->bytes in)))
    (define est-size (string->number (bytes->string/utf-8 (extract-field #"content-length" headers))))
    (make-link
     (make-input-file (get-pure-port #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS) (string->url source))
                      est-size))))


(define (fetch-source/xiden-query source make-input-file build-derivation make-link)
  (void)
  #;(with-handlers ([exn:fail? (λ (e) #f)])
    (define query (string->xiden-query source))
    (define urls
      (map (λ (url-string)
             (define u (string->url url-string))
             (url->string
              (struct-copy url u
                           [path
                            (append (url-path u)
                                    (list (path/param source null)))])))
           (XIDEN_SERVICE_ENDPOINTS)))


    (define pkginfo
      (for/or ([u (in-list urls)])
        (read-package-info (get-pure-port u #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS)))))

    (define input-paths
      (for/list ([input (in-list (package-info-inputs pkginfo))])
        (fulfill-input input)))

    (define derivation-paths
      (for/list ([output (in-list (package-info-outputs pkginfo))])
        (build-derivation (find-setup-module pkginfo output)
                          (output-info-builder-expressions output)
                          (input-paths))))))


(define (fetch-source input source)
  (define method
    (disjoin fetch-source/filesystem
             fetch-source/http
             fetch-source/xiden-query
             (load-plugin 'fetch-source
                          (λ () #f) (λ (e) #f))))

  (define (make-file in est-size)
    (make-input-file input source in est-size))

  (define (make-link path)
    (make-file-or-directory-link path (input-info-name input)))

  (define input-path
    (method make-file make-link))

  (or input-path
      (rex)))


(define (raise-unless-postconditions-met input source tmp)
  (unless (XIDEN_TRUST_BAD_DIGEST)
    (or (check-integrity (input-info-integrity input) tmp)
        (rex exn:fail:xiden:source:digest-mismatch input source)))
    (unless (XIDEN_TRUST_UNSIGNED)
      (or (check-signature (integrity-info-digest (input-info-integrity input))
                           (signature-info-body (input-info-signature input))
                           (signature-info-pubkey (input-info-signature input)))
          (rex exn:fail:xiden:source:signature-mismatch input source))))


(define (mibibytes->bytes mib)
  (inexact->exact (ceiling (* mib 1024 1024))))

(define (make-input-file input source in est-size)
  (define tmp (make-temporary-file))
  (dynamic-wind
    void
    (λ ()
      (with-handlers ([values (λ (e) (delete-file* tmp) (raise e))])
        (define bytes-written
          (call-with-output-file tmp #:exists 'truncate/replace
            (λ (to-file)
              (transfer in to-file
                        #:on-progress (λ (name scalar)
                                        (write-output ($fetch-progress name scalar)))
                        #:transfer-name (input-info-name input)
                        #:max-size (mibibytes->bytes (XIDEN_FETCH_TOTAL_SIZE_MB))
                        #:buffer-size (mibibytes->bytes (XIDEN_FETCH_BUFFER_SIZE_MB))
                        #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
                        #:est-size est-size))))

        (raise-unless-postconditions-met input source tmp)
        (define path (build-input-path (make-digest tmp (integrity-info-algorithm (input-info-integrity input)))))
        (make-directory* (path-only path))
        (rename-file-or-directory tmp path)
        path))
    (λ () (close-input-port in))))


(module+ test
  (require racket/runtime-path
           rackunit
           mzlib/etc
           (submod "file.rkt" test)
           "setting.rkt")

  (define me (build-path (this-expression-source-directory) (this-expression-file-name)))


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
