#lang racket/base

(require racket/contract
         racket/format
         racket/runtime-path
         racket/string
         (rename-in ffi/unsafe [-> -->])
         "message.rkt")

(provide (struct-out $crypto)
         (struct-out $crypto:error)
         (struct-out $crypto:unavailable)
         (contract-out
          [crypto-clear-ffi-cache!
           (-> void?)]
          [crypto-get-lib!
           (-> (or/c ffi-lib? exn?))]
          [crypto-get-obj!
           (-> (or/c bytes? symbol? string?)
               ctype?
               any/c)]
          [crypto-dump-error-queue!
           (-> (or/c #f list?))]
          [crypto-translate-error!
           (-> exact-integer?
               (or/c #f string?))]
          [assert-crypto-availability
           procedure?]
          [crypto-raise!
           procedure?]))


(define-message $crypto ())
(define-message $crypto:error $crypto (queue))
(define-message $crypto:unavailable $crypto ())
(define-runtime-path crypto/ "crypto")

(define ffi-cache (make-hash))

(define (crypto-clear-ffi-cache!)
  (hash-clear! ffi-cache))

(define (crypto-get-lib!)
  (define key '||)

  (define (attempt!)
    (define arch
      (let ([subpath (~a (system-library-subpath))])
        (if (equal? (system-path-convention-type) 'windows)
            (cadr (string-split subpath "\\"))
            (car (string-split subpath "-")))))

    (define local-directory-name
      (~a arch "-" (system-type 'os)))

    (hash-set! ffi-cache key
               (with-handlers ([values values])
                 (ffi-lib
                  (path-replace-extension
                   (build-path crypto/
                               local-directory-name
                               "crypto")
                   (system-type 'so-suffix)))))

    (hash-ref ffi-cache key))

  (if (hash-has-key? ffi-cache key)
      (if (exn? (hash-ref ffi-cache key))
          (attempt!)
          (hash-ref ffi-cache key))
      (attempt!)))

(define (crypto-get-obj! sym type)
  (define l (crypto-get-lib!))
  (hash-ref! ffi-cache sym
             (λ ()
               (and (not (exn? l))
                    (get-ffi-obj sym l type)))))

(define (assert-crypto-availability)
  (when (exn? (crypto-get-lib!))
    (raise ($crypto:unavailable))))

(define (crypto-dump-error-queue!)
  (define get (crypto-get-obj! #"ERR_get_error" (_fun --> _ulong)))
  (and get
       (let loop ([q null])
         (define code (get))
         (if (zero? code)
             (reverse q)
             (loop (cons code q))))))


(define crypto-translate-error!
  (let ([translation-buffer (make-bytes 256)])
    (λ (code)
      (define translate
        (crypto-get-obj! #"ERR_error_string"
                         (_fun _ulong _pointer --> _string)))
      (and translate
           (begin (translate code translation-buffer)
                  (bytes->string/utf-8 translation-buffer))))))


(define (crypto-raise!)
  (raise ($crypto:error (crypto-dump-error-queue!))))
