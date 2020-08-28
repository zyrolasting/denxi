#lang racket/base

; Define means for fetching bytes from some origin.

(require "contract.rkt")
(provide (struct-out fetch-info)
         (contract-out
          [fetch-source
           (-> string?
               (-> input-port?
                   (or/c +inf.0 exact-positive-integer?)
                   any/c)
               any/c)]
          [transfer-package-info
           (-> input-port?
               (or/c +inf.0 exact-positive-integer?)
               ($with-messages/c package-info?))]
          [well-formed-fetch-info/c
           flat-contract?]))

; Define a message space to capture what goes wrong or right
; when trying to procure bytes.

(require "message.rkt")
(define+provide-message $source (user-string))
(define+provide-message $source-method-ruled-out $source (reason))
(define+provide-message $source-fetched $source ())
(define+provide-message $source-unfetched $source ())
(define+provide-message $fetch (info))
(define+provide-message $fetch-integrity $fetch (source))
(define+provide-message $fetch-integrity-assumed  $fetch-integrity ())
(define+provide-message $fetch-integrity-mismatch $fetch-integrity ())
(define+provide-message $fetch-integrity-verified $fetch-integrity ())
(define+provide-message $fetch-signature $fetch (source))
(define+provide-message $fetch-signature-mismatch $fetch-signature ())
(define+provide-message $fetch-signature-missing $fetch-signature ())
(define+provide-message $fetch-signature-trust-unsigned $fetch-signature ())
(define+provide-message $fetch-signature-unchecked $fetch-signature ())
(define+provide-message $fetch-signature-verified $fetch-signature ())


; Implementation follows

(require racket/function
         net/head
         "config.rkt"
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
         "rc.rkt"
         "signature.rkt"
         "string.rkt"
         "url.rkt")


(struct fetch-info
  (name       ; The name of the link used to reference input bytes
   sources    ; Where to look to get bytes
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)


(define well-formed-fetch-info/c
  (struct/c fetch-info
            non-empty-string?
            (non-empty-listof any/c)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))


(define (fetch* infos request-transfer)
  (for/fold ([links (:return (hash))])
            ([info (in-list infos)])
    (define fetch-res (fetch info request-transfer))
    (:merge fetch-res
            (:return
             (hash-set ($with-messages-intermediate links)
                       (fetch-info-name info)
                       ($with-messages-intermediate fetch-res))))))


(define (fetch info request-transfer)
  (apply :do (map (λ (source)
                    (fetch-exact-source info
                                        (λ args
                                          (apply request-transfer
                                                 (fetch-info-name info)
                                                 args))))
                  (get-fetch-sources info))))


(define (fetch-exact-source info source request-transfer)
  (:do #:with (fetch-source source request-transfer)
       (λ (path) (check-fetch-integrity info source path))
       (λ (path) (check-fetch-signature info source path))))


(define (fetch-source source request-transfer)
  (define (mod-fallback . _) (const #f))
  ; The fetch procedures can just return #f if they cannot
  ; find something. This instruments the procedures so that
  ; they can be composed below.
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
                  (attach-message #f
                   ($source-method-ruled-out source
                                             (format "Method produced nothing: ~a"
                                                     method-name)))))))))

  (:do #:with (:return #f)
       (lift-fetch-source-method fetch-source/filesystem "Filesystem")
       (lift-fetch-source-method fetch-source/http "HTTP")
       (lift-fetch-source-method fetch-source/xiden-query "Package query")
       (lift-fetch-source-method (load-plugin 'fetch-source mod-fallback mod-fallback) "Plugin")
       (λ (maybe-path)
         (attach-message maybe-path
                         (if maybe-path
                             ($source-fetched source)
                             ($source-unfetched source))))))


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


; I add any cached path as a source so that it goes through
; validation. This captures local tampering.
(define (get-fetch-sources info)
  (with-handlers ([exn? (λ _ (fetch-info-sources info))])
    (define path
      (build-object-path
       (integrity-info-digest
        (fetch-info-integrity info))))
    (if (file-exists? path)
        (cons (path->string path)
              (fetch-info-sources info))
        (fetch-info-sources info))))


(define (check-fetch-integrity info source path)
  (if (XIDEN_TRUST_BAD_DIGEST)
      (attach-message path ($fetch-integrity-assumed info source))
      (if (check-integrity (fetch-info-integrity info) path)
          (attach-message path ($fetch-integrity-verified info source))
          (raise (attach-message #f ($fetch-integrity-mismatch info source))))))


(define (check-fetch-signature info source path)
  (if (XIDEN_TRUST_BAD_DIGEST)
      (attach-message path ($fetch-signature-unchecked info source))
      (if (XIDEN_TRUST_UNSIGNED)
          (attach-message path ($fetch-signature-trust-unsigned info source))
          (if (check-signature (integrity-info-digest (fetch-info-integrity info))
                               (signature-info-body (fetch-info-signature info))
                               (signature-info-pubkey (fetch-info-signature info)))
              (attach-message path ($fetch-signature-verified info source))
              (raise (attach-message #f ($fetch-signature-mismatch info source)))))))




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

    (define fetch-output
      (fetch-source source
                    (λ (in est-size)
                      (check-eq? est-size 5)
                      (check-equal? (read-bytes est-size in) #"12345")
                      (kill-thread th)
                      (tcp-close listener))))

    (check-equal? (find-message $source-fetched? fetch-output)
                  ($source-fetched source))))
