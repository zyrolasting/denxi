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
(define+provide-message $source-method-ruled-out $source (reason))
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
  (and (file-exists? source)
       (make-file (open-input-file source)
                  (+ (* 20 1024) ; for Mac OS resource forks
                     (file-size source)))))


(define (fetch-source/http source make-file)
  (define in (head-impure-port (string->url source)))
  (define headers (extract-all-fields (port->string in)))
  (define content-length-pair (assf (λ (el) (equal? (string-downcase el) "content-length")) headers))
  (define est-size
    (if content-length-pair
        (string->number (or (cdr content-length-pair) "+inf.0"))
        "+inf.0"))
  (make-file (get-pure-port #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS) (string->url source))
             est-size))


(define (fetch-source/xiden-query source make-file)
  (define pkginfo
    (for/or ([u (in-list (map/service-endpoints source (XIDEN_SERVICE_ENDPOINTS)))])
      (read-package-info (get-pure-port u #:redirections (XIDEN_DOWNLOAD_MAX_REDIRECTS)))))
  (and pkginfo
       (let-values ([(i o) (make-pipe)])
         (write-config #:pretty? #t (package-info->hash pkginfo) null o)
         (flush-output o)
         (close-output-port o)
         (make-file i (mibibytes->bytes (XIDEN_FETCH_PKGDEF_SIZE_MB))))))


(define (fetch-source source request-transfer)
  (define (mod-fallback . _) (const #f))


  ; The fetch procedures can just return #f if they cannot
  ; find something. This instruments the procedures so that
  ; they can be composed in fetch-source.
  (define (lift-fetch-source-method f method-name)
    (λ (status)
      (if status
          (:return status)
          (with-handlers
            ([exn:fail?
              (λ (e)
                (attach-message #f ($source-method-ruled-out source (exn->string e))))])
            (let ([maybe-result (f source request-transfer)])
              (if maybe-result
                  (:return maybe-result)
                  (attach-message
                   #f
                   ($source-method-ruled-out source
                                             (format "Method produced nothing: ~a"
                                                     method-name)))))))))

  (:do #:with (:return (fetch-source/filesystem source request-transfer))
       (lift-fetch-source-method fetch-source/http "HTTP")
       (lift-fetch-source-method fetch-source/xiden-query "Package query")
       (lift-fetch-source-method (load-plugin 'fetch-source mod-fallback mod-fallback) "Plugin")
       (λ (maybe-path)
         (attach-message maybe-path
                         (if maybe-path
                             ($source-fetched source)
                             ($source-unfetched source))))))


(define (fetch-inputs inputs)
  (apply :do #:with (hash)
         (for/list ([input (in-list inputs)])
           (λ (links)
             (define fetch-res (fetch-input input))
             (:merge fetch-res
                     (:return
                      (hash-set links
                                (input-info-name input)
                                ($with-messages-intermediate fetch-res))))))))


(define (fetch-input input request-transfer)
  ; I add any cached path as a source so that it goes through
  ; validation. This captures local tampering.
  (define existing (get-maybe-existing-input-path input))
  (apply :do (map (λ (source) (fetch-exact-source input source))
                  (if existing
                      (cons (path->string existing)
                            (input-info-sources input))
                      (input-info-sources input)))))


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
  (require racket/file
           racket/tcp
           rackunit
           "setting.rkt")

  (define plugin-module-datum
    '(module mods racket/base
       (provide fetch-source)
       (define (fetch-source source make-file)
         (make-file (open-input-string source)
                    (string-length source)))))

  (define mod-source "====[{]::[}]====")

  (define (try-mod-fetch)
    (fetch-source mod-source
                  (λ (in est-size)
                    (check-equal? est-size (string-length mod-source))
                    (check-equal? (read-string est-size in) mod-source))))

  (call-with-temporary-file
   (λ (tmp)
     (write-to-file plugin-module-datum tmp #:exists 'truncate/replace)
     (parameterize ([(setting-derived-parameter XIDEN_MODS_MODULE) tmp])
       (define source (path->string tmp))
       (test-equal? "Fetch from file"
                    (fetch-source
                     source
                     (λ (in est-size)
                       (test-equal? "Can read file" (read in) plugin-module-datum)
                       (test-true "Can estimate file size" (>= est-size (file-size tmp)))
                       'some-value))
                    (attach-message 'some-value ($source-fetched source)))

       (test-equal? "Fetch from mod"
                    (find-message $source-fetched? (try-mod-fetch))
                    ($source-fetched mod-source)))))

  ; Notice we just left the parameterize that set the mod path
  (test-case "Investigate fetch failures"
    (define fetch-output (try-mod-fetch))
    (test-equal? "Fetch fails when mod is not available"
                 (find-message $source-unfetched? fetch-output)
                 ($source-unfetched mod-source))
    (check-false (null? (filter $source-method-ruled-out? ($with-messages-accumulated fetch-output)))))

  (test-case "Fetch over HTTP"
    (define listener (tcp-listen 8018 1 #t))
    (define th
      (thread
       (λ ()
         (let loop ([num 0])
           (define-values (in out) (tcp-accept listener))
           (display "HTTP/1.1 200 OK\r\n" out)
           (display "https: //www.example.com/\r\n" out)
           (display "Content-Type: text/html; charset=UTF-8\r\n" out)
           (display "Date: Fri, 28 Aug 2020 04:02:21 GMT\r\n" out)
           (display "Server: foo\r\n" out)
           (display "Content-Length: 5\r\n\r\n" out)
           (unless (= num 0)
             (display "12345" out))
           (flush-output out)
           (close-output-port out)
           (close-input-port in)
           (loop (add1 num))))))

    (define source "http://127.0.0.1:8018")
    (check-equal?
     (fetch-source source
                   (λ (in est-size)
                     (check-eq? est-size 5)
                     (check-equal? (read-bytes est-size in) #"12345")
                     (kill-thread th)
                     (tcp-close listener)))
     (attach-message (void)
                     ($source-fetched source)))))
