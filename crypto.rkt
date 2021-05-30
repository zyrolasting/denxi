#lang racket/base

(require racket/contract
         racket/format
         racket/list
         racket/port
         racket/runtime-path
         ffi/unsafe/atomic
         ffi/unsafe/define
         ffi/vector
         (rename-in ffi/unsafe [-> -->])
         "message.rkt"
         "setting.rkt"
         "codec.rkt")

(define md-bytes-source/c
  (or/c path-string? bytes? input-port?))

(provide (contract-out
          [cryptographic-hash-functions
           (listof symbol?)]
          [chf/c
           flat-contract?]
          [DEFAULT_CHF
            chf/c]
          [md-bytes-source/c
           flat-contract?]
          [make-digest
           (->* (md-bytes-source/c)
                (chf/c)
                bytes?)]))

(define-runtime-path crypto.so "crypto/crypto.so")
(define-runtime-path crypto.dll "crypto/crypto.dll")
(define-runtime-path crypto.dynlib "crypto/crypto.dynlib")

(define+provide-message $crypto ())
(define+provide-message $crypto:make-digest-failure $crypto (error-code))


(define crypto-lib-file
  (case (system-type 'os)
    [(windows) crypto.dll]
    [(unix) crypto.so]
    [(macosx) crypto.dynlib]))

(define (chf/c v)
  (and (member v cryptographic-hash-functions) #t))

(define+provide-setting XIDEN_TRUST_CHFS (listof chf/c) null)

(define-ffi-definer define-crypto (ffi-lib crypto-lib-file))

(define-crypto make_digest_unsafe
  (_fun _pointer
        _pointer
        (_fun _pointer --> _u8vector)
        --> _int))

(define-crypto get_chf_handle (_fun _uint --> _pointer))
(define-crypto get_digest_size (_fun _pointer --> _int))
(define-crypto XIDEN_AVAILABLE_CHF_COUNT _uint)
(define-crypto XIDEN_SUPPORTED_CHFS (_array _string XIDEN_AVAILABLE_CHF_COUNT))
(define-crypto XIDEN_DEFAULT_CHF_INDEX _uint)

(define cryptographic-hash-functions
  (let* ([arr XIDEN_SUPPORTED_CHFS])
    (for/list ([i (in-range XIDEN_AVAILABLE_CHF_COUNT)])
      (string->symbol (string-downcase (array-ref arr i))))))

(define DEFAULT_CHF
  (list-ref cryptographic-hash-functions
            XIDEN_DEFAULT_CHF_INDEX))

(define (make-digest variant [algorithm DEFAULT_CHF])
  (cond [(path-string? variant)
         (call-with-input-file* (expand-user-path variant)
           (λ (i) (make-digest i algorithm)))]
        [(bytes? variant)
         (make-digest (open-input-bytes variant) algorithm)]
        [(input-port? variant)
         (define mdindex (index-of cryptographic-hash-functions algorithm))
         (unless mdindex (error "Invalid index"))
         (define handle (get_chf_handle mdindex))
         (unless handle (error "Invalid handle"))
         (define digest-size (get_digest_size handle))
         (define read-buffer-size (* 128 1024))
         (define gc-buffer (make-bytes read-buffer-size))
         (define digest-buffer (make-bytes digest-size))
         (define error-code
           (call-as-atomic
            (λ ()
              (make_digest_unsafe handle
                                  digest-buffer
                                  (λ (pSize)
                                    (define bytes-read
                                      (read-bytes! gc-buffer
                                                   variant
                                                   0
                                                   read-buffer-size))
                                    (and (not (eof-object? bytes-read))
                                         (begin (ptr-set! pSize _uint bytes-read)
                                                gc-buffer)))))))

         (unless (equal? error-code 0)
           (raise ($crypto:make-digest-failure error-code)))

         digest-buffer]))

(module+ test
  (require rackunit
           "codec.rkt")

  (test-pred (format "Make ~a-specific library available"
                     (system-type 'os))
             file-exists?
             crypto-lib-file)

  (test-case "Load CHF data from library"
    (check-true
     (for/and ([s (in-list cryptographic-hash-functions)])
       (symbol? s)))
    (check-not-false
     (member DEFAULT_CHF cryptographic-hash-functions)))


  (define data #"the rain in spain falls mainly on the plain\n")
  (define digests
    '((sha3-384 . "C5cwjJB4SzrDF0DDwfxIQeTwulzTMGSpKSZmW1BSzEs4GLQJy5emOmigwBAO5DUY")
      (sha256 . "2AQkOghwLerAkbcbvdWrtocTeBXV5UnbPA56MsyClqk=")
      (sha1 . "89mymPTvJIenhzZpwZ7uDKUANd8=")
      (md5 . "K0/4xUnpQ4mZjePs8CJjfw==")
      (sha2-224 . "J6HLdwlQa7vmdBXI+0rIbmk0Q0phlLYoHUO7Fw==")
      (sha2-384 . "V5oG04vbEMtqMkN7yHQk3Rw53O9PE3P7UrN7Fr5NV2hnRQkfotR06+D/XWh58dVK")
      (sha3-224 . "4UEGL2680oczBfoD+4SoH3k82OeevWq8b7seDw==")
      (sha3-256 . "dHGOrfdKJB1Agz8mMafKamWzJg7Cu1Cg1G3XM3vWFJc=")
      (sha3-384 . "C5cwjJB4SzrDF0DDwfxIQeTwulzTMGSpKSZmW1BSzEs4GLQJy5emOmigwBAO5DUY")
      (sha3-512 . "UQqbIueWYFqdA9ibHPS2O0l+gX9DPySh4KPIv97XDBHPlPZB5MMzN4mXamWIuTLxLaZO8r2euZx0tfoksyevaQ==")))

  (for ([pair (in-list digests)])
    (define chf (car pair))
    (define expected-digest (base64 (cdr pair)))
    (test-equal? (format "Make digest using ~a" chf)
                 (make-digest data chf)
                 expected-digest)))
