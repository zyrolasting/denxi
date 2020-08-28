#lang racket/base

; Define a derivation and the operations used to create
; unique and internally-consistent directories on disk.

(require "contract.rkt")

(provide fetch-source
         transfer-package-info
         make-sandbox
         (contract-out
          [build-derivation
           (-> well-formed-output-info/c
               list?)]))


(require racket/function
         racket/generator
         racket/path
         racket/pretty
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
         "input-info.rkt"
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
         "string.rkt"
         "url.rkt"
         "openssl.rkt"
         "output-info.rkt"
         "workspace.rkt")


(define+provide-message $consent-note ())
(define+provide-message $input (info))
(define+provide-message $input-integrity $input (source))
(define+provide-message $input-integrity-assumed  $input-integrity ())
(define+provide-message $input-integrity-mismatch $input-integrity ())
(define+provide-message $input-integrity-verified $input-integrity ())
(define+provide-message $input-signature $input (source))
(define+provide-message $input-signature-mismatch $input-signature ())
(define+provide-message $input-signature-missing $input-signature ())
(define+provide-message $input-signature-trust-unsigned $input-signature ())
(define+provide-message $input-signature-unchecked $input-signature ())
(define+provide-message $input-signature-verified $input-signature ())
(define+provide-message $no-package-info (source))
(define+provide-message $sandbox ())
(define+provide-message $sandbox-crash (exn-string))
(define+provide-message $sandbox-error (line))
(define+provide-message $sandbox-output (line))
(define+provide-message $source (user-string))
(define+provide-message $source-fetched $source ())
(define+provide-message $source-unfetched $source ())
(define+provide-message $unverified-host (url))

(define (mibibytes->bytes mib)
  (inexact->exact (ceiling (* mib 1024 1024))))

(struct derivation (inputs output) #:prefab)

(define (build-derivation drv)
  (:do #:with (fetch-inputs (derivation-inputs drv))
       (λ (input-bindings) (make-refs input-bindings))
       (λ (_) (run-builder (derivation-output drv)))))


(define (make-refs bindings)
  (:merge (for/fold ([accum (:return)])
                    ([(link-name target-path) (in-hash bindings)])
            (:merge accum (make-link/clobber link-name target-path)))
          (:return (void))))


(define (run-builder output)
  (define name (output-info-builder-name output))
  (define seval (make-sandbox name))
  (define pump-stdout (thread (make-pump $sandbox-error name (get-output seval))))
  (define pump-stderr (thread (make-pump $sandbox-error name (get-error-output seval))))
  (dynamic-wind void
                (λ () (collect-sandbox-eval-results seval (output-info-builder-expressions output)))
                (λ ()
                  (break-thread pump-stdout)
                  (break-thread pump-stderr))))


(define (collect-sandbox-eval-results seval expressions)
  (for/fold ([msg (:return)])
            ([expr (in-list expressions)]
             #:break ($with-messages-intermediate msg))
    (:merge msg
            (with-handlers ([values (λ (e) (attach-message #t ($sandbox-crash (exn->string e))))])
              (attach-message #f ($sandbox-output (seval expr)))))))


(define (make-pump message-envelope name port)
  (λ ()
    (with-handlers ([exn:break? (λ (e) (close-input-port port))])
      (let loop ()
        (emit-message! (message-envelope (read-line (sync port))))
        (loop)))))


(define (make-sandbox path)
  (parameterize ([sandbox-output 'pipe]
                 [sandbox-error-output 'pipe]
                 [sandbox-input #f]
                 [sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-path-permissions (XIDEN_SANDBOX_PATH_PERMISSIONS)]
                 [sandbox-make-environment-variables make-environment-variables])
    (make-module-evaluator #:language 'racket/base path)))


(define (fetch-source/filesystem source make-file)
  (with-handlers ([exn:fail? (λ (e) ($show-string (exn->string e)))])
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
    (for/or ([u (in-list (map/service-endpoints source (XIDEN_SERVICE_ENDPOINTS)))])
      (with-handlers ([exn:fail? (λ (e) #f)])
        (read-package-info (get-pure-port u #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS))))))
  (and pkginfo
       (let-values ([(i o) (make-pipe)])
         (write-config #:pretty? #t (package-info->hash pkginfo) null o)
         (flush-output o)
         (close-output-port o)
         (make-file i (mibibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB))))))


(define (get-fetch-source-method)
  (disjoin fetch-source/filesystem
           fetch-source/http
           fetch-source/xiden-query
           (load-plugin 'fetch-source
                        (λ () (const #f))
                        (λ (e) (const #f)))))


(define (fetch-source source request-transfer)
  (define maybe-path ((get-fetch-source-method) source request-transfer))
  (attach-message maybe-path
                  (if maybe-path
                      ($source-fetched source)
                      ($source-unfetched source))))


(define (fetch-input input request-transfer)
  ; I add any cached path as a source so that it goes through
  ; validation. This captures local tampering.
  (define existing (get-maybe-existing-input-path input))
  (apply :do (map (λ (source) (fetch-exact-source input source))
                  (if existing
                      (cons (path->string existing)
                            (input-info-sources input))
                      (input-info-sources input)))))


(define (fetch-inputs inputs)
  (apply :do #:with (hash)
         (for/list ([input (in-list inputs)])
           (λ (links)
             (hash-set links
                       (input-info-name input)
                       ($with-messages-intermediate
                        (fetch-input input)))))))


(define (get-maybe-existing-input-path input)
  (with-handlers ([exn? (λ _ #f)])
    (define path
      (build-object-path
       (integrity-info-digest
        (input-info-integrity input))))
    (and (file-exists? path)
         path)))


(define (fetch-exact-source input source request-transfer)
  (:do #:with (fetch-source source request-transfer)
       (λ (path) (check-input-integrity input source path))
       (λ (path) (check-input-signature input source path))))


(define (check-input-integrity input source path)
  (if (XIDEN_TRUST_BAD_DIGEST)
      (attach-message path ($input-integrity-assumed input source))
      (if (check-integrity (input-info-integrity input) path)
          (attach-message path ($input-integrity-verified input source))
          (raise (attach-message #f ($input-integrity-mismatch input source))))))


(define (check-input-signature input source path)
  (if (XIDEN_TRUST_BAD_DIGEST)
      (attach-message path ($input-signature-unchecked input source))
      (if (XIDEN_TRUST_UNSIGNED)
          (attach-message path ($input-signature-trust-unsigned input source))
          (if (check-signature (integrity-info-digest (input-info-integrity input))
                               (signature-info-body (input-info-signature input))
                               (signature-info-pubkey (input-info-signature input)))
              (attach-message path ($input-signature-verified input source))
              (raise (attach-message #f ($input-signature-mismatch input source)))))))


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


(module+ test
  (require racket/file rackunit)

  (test-case "Create a directory tree using a derivation"

    ))
