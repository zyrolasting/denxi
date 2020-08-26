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
         "input-forms-lang.rkt"
         "localstate.rkt"
         "message.rkt"
         "mod.rkt"
         "output.rkt"
         "package-info.rkt"
         "path.rkt"
         "port.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "string.rkt"
         "url.rkt"
         "openssl.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")

(define-exn exn:fail:xiden:source exn:fail:xiden (source))
(define-exn exn:fail:xiden:source:no-content exn:fail:xiden:source ())
(define-exn exn:fail:xiden:source:digest-mismatch exn:fail:xiden:source ())
(define-exn exn:fail:xiden:source:signature-mismatch exn:fail:xiden:source ())

(define-message $no-source-bytes (source))

(define input-forms-namespace
  (module->namespace "input-forms-lang.rkt"))

(define (in-referenced-package-infos pkginfo)
  (define (yield-package-infos pkginfo)
    (yield pkginfo)
    (for ([input-expr (in-list (package-info-inputs pkginfo))])
        (define path (fetch-input (eval input-expr input-forms-namespace)))
        (yield-package-infos (read-package-info path))))
  (in-generator
   (call-with-applied-settings
    (hash XIDEN_FETCH_TOTAL_SIZE_MB (XIDEN_FETCH_PKGDEF_SIZE_MB)
          XIDEN_FETCH_BUFFER_SIZE_MB (max (/ (XIDEN_FETCH_PKGDEF_SIZE_MB) 5) 5))
    (λ () (yield-package-infos pkginfo)))))



(define (mibibytes->bytes mib)
  (inexact->exact (ceiling (* mib 1024 1024))))

(define (map/service-endpoints to-add)
  (map (λ (url-string)
         (define u (string->url url-string))
         (url->string
          (struct-copy url u
                       [path
                        (append (url-path u)
                                (list (path/param to-add null)))])))
       (XIDEN_SERVICE_ENDPOINTS)))


(define (make-package-path pkginfo)
  (build-workspace-path (XIDEN_INSTALL_RELATIVE_PATH)
                        (make-package-name pkginfo)))


(define (install-package! pkginfo)
  (define path (make-package-path pkginfo))
  (if (directory-exists? path)
      (:fail ($already-installed path))
      (run-package! pkginfo path)))


(define (run-package! pkginfo path)
  (make-directory* path)
  (parameterize ([current-directory path])
    (:fold pkginfo
           (list fetch-inputs!
                 build-outputs!))))


(define (fetch-inputs! inputs)
  (:fold (hash)
         (for/list ([input (in-list inputs)])
           (λ (links)
             (:unit
              (hash-set links
                        (input-info-name input)
                        ($with-output-intermediate (fetch-input input))))))))


(define (build-outputs! pkginfo)
  (:fold null
         (for/list ([output (in-list (package-info-outputs pkginfo))])
           (λ (sandbox-values)
             (cons (build-derivation output)
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
           (:fail ($undeclared-racket-version pkginfo)))])))


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


(define (build-derivation output)
  (define seval (make-sandbox (output-info-builder-name output)))
  (for/list ([expr (in-list (output-info-builder-expressions output))])
    (seval expr)))


(define (fetch-source/filesystem source make-file)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (and (file-exists? source)
         (make-file (open-input-file source)
                    (+ (* 20 1024) ; for Mac OS resource forks
                       (file-size source))))))


(define (fetch-source/http source make-file)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define in (head-impure-port (string->url source)))
    (define headers (extract-all-fields (port->bytes in)))
    (define est-size (string->number (bytes->string/utf-8 (extract-field #"content-length" headers))))
    (make-file (get-pure-port #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS) (string->url source))
               est-size)))


(define (fetch-source/xiden-query source make-file)
  (define pkginfo
    (for/or ([u (in-list (map/service-endpoints source))])
      (with-handlers ([exn:fail? (λ (e) #f)])
        (read-package-info (get-pure-port u #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS))))))

  (and pkginfo
       (let-values ([(i o) (make-pipe)])
         (write-config #:pretty? #t (package-info->hash pkginfo) null o)
         (flush-output o)
         (close-output-port o)
         (make-file i (mibibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB))))))


(define (get-fetch-source-method source)
  (disjoin fetch-source/filesystem
           fetch-source/http
           fetch-source/xiden-query
           (load-plugin 'fetch-source
                        (λ () (const #f))
                        (λ (e) (const #f)))))


(define (fetch-source source)
  (define method
    (get-fetch-source-method))

  (define maybe-path
    (method source
            (λ (make-file in est-size)
              (make-addressable-file source in est-size))))

  (if maybe-path
      (:unit maybe-path)
      ($no-source-bytes source)))



#;(define (make-link path . others)
  (if (null? others)
      (make-file-or-directory-link path (input-info-name input))
      (begin (make-directory* (input-info-name input))
             (for ([other (in-list others)])
               (make-file-or-directory-link path
                                            (build-path (input-info-name input) other))))))


(define (raise-unless-postconditions-met input source tmp)
  (unless (XIDEN_TRUST_BAD_DIGEST)
    (or (check-integrity (input-info-integrity input) tmp)
        (rex exn:fail:xiden:source:digest-mismatch input source)))
    (unless (XIDEN_TRUST_UNSIGNED)
      (or (check-signature (integrity-info-digest (input-info-integrity input))
                           (signature-info-body (input-info-signature input))
                           (signature-info-pubkey (input-info-signature input)))
          (rex exn:fail:xiden:source:signature-mismatch input source))))


(define (fetch-input input)
  (call/cc
   (λ (return)
     (define existing (get-maybe-existing-input-path input))
     (if existing
         existing
         (for ([source (in-list (input-info-sources input))])
           (with-handlers ([exn? void])
             (define candidate-path (fetch-source source))
             (raise-unless-postconditions-met input source candidate-path)
             (return candidate-path)))))))



(define (get-maybe-existing-input-path input)
  (with-handlers ([exn? (λ _ #f)])
    (define path
      (build-object-path
       (integrity-info-digest
        (input-info-integrity input))))
    (and (file-exists? path)
         path)))


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
                  ($with-output 1 #f (list ($undeclared-racket-version pkginfo)))))

  (test-case "Detect packages that declare an unsupported Racket version"
    (define pkginfo
      (make-package-info #:provider-name "provider"
                         #:package-name "pkg"
                         #:racket-versions (list "0.0")))

    (check-equal? (check-racket-support pkginfo)
                  ($with-output 1 #f (list ($unsupported-racket-version pkginfo))))))
