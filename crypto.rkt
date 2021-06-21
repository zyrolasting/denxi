#lang racket/base

(require racket/contract
         racket/file
         racket/format
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         ffi/vector
         (rename-in ffi/unsafe [-> -->])
         "message.rkt"
         "setting.rkt"
         "codec.rkt")

(define+provide-message $crypto ())
(define+provide-message $crypto:error $crypto (queue))

(provide
 (contract-out
  [cryptographic-hash-functions (listof symbol?)]
  [chf/c flat-contract?]
  [DEFAULT_CHF chf/c]
  [sign-with-snake-oil
   (->* (bytes?)
        (chf/c)
        bytes?)]
  [make-digest
   (->* ((or/c bytes? path-string? input-port?))
        (chf/c)
        bytes?)]
  [verify-signature
   (-> bytes?
       chf/c
       bytes?
       bytes?
       boolean?)]
  [snake-oil-private-key bytes?]
  [snake-oil-public-key bytes?]
  [snake-oil-private-key-password bytes?]
  [get-crypto-error-strings
   (-> (or/c (listof exact-positive-integer?) $crypto:error?)
       (listof string?))]
  [make-signature
   (->* (bytes?
         chf/c
         bytes?)
        ((or/c #f bytes?))
        bytes?)]))



;-------------------------------------------------------------------------------
; Snake oil data for use in prototyping.

(define snake-oil-private-key-password #"foobar")

(define snake-oil-private-key (string->bytes/utf-8 #<<EOF
-----BEGIN RSA PRIVATE KEY-----
Proc-Type: 4,ENCRYPTED
DEK-Info: AES-128-CBC,E1A24462AABA790A82015D69D2548D52

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
-----END RSA PRIVATE KEY-----
EOF
))


(define snake-oil-public-key (string->bytes/utf-8 #<<EOF
-----BEGIN PUBLIC KEY-----
MIIBojANBgkqhkiG9w0BAQEFAAOCAY8AMIIBigKCAYEA0Damo9DNCiQcgYmcUsY/
Gm3Y3fQIQseUuuwphnnleTl9/9m7F4KEIsj+f4vCFbcPc1iiKh8d9UE+brF2ShjR
QbggsPPjUtfMF8Z9HiokIkNf2uM4uduBuzoejgqyLpkr6fLFyKxh0a+nwB+O6hLI
8zJnt0BJZhA0pYds0eZJLdwArjJ4fi3H7q+64tSlHSXBQ+FETcBpk4bNTrSHIKYy
0FX7k5kW9QLlZ3Kr33x06TAROC92pyKSJrXYmN/85wDlZs0mTMmsc6M3tbAzDEWn
0ef0BCCowNgJunrzKzbEOHEcxmA8Y6bstPeasBL4W6/Vm3udRHueIE5N5ZQEbOhw
HcQcXTapPM3yw9sWO28pKGZKPTs+uqEsuFk3PfTWbpEmm/OLnx7PYsoHFlVvp0d/
h9a1LhC00b6/p9132U4JiRRHdGsWagVD7dwHahuWUT1Kx+uzkT01lJwzucurKtzX
1L31+I0V/bhUAvsFpuPA99jWRS3zoynA3/8ESHslLtU9AgMBAAE=
-----END PUBLIC KEY-----
EOF
))

;-------------------------------------------------------------------------------
; FFI

(define-runtime-path crypto/ "crypto")

(define arch
  (let ([subpath (~a (system-library-subpath))])
    (if (equal? (system-path-convention-type) 'windows)
        (cadr (string-split subpath "\\"))
        (car (string-split subpath "-")))))

(define crypto-lib
  (ffi-lib
   (path-replace-extension
    (build-path crypto/
                (~a arch "-" (system-type 'os))
                "crypto")
    (system-type 'so-suffix))))

(define _EVP_MD-pointer _pointer)

(define (get-ffi-obj* sym type)
  (get-ffi-obj sym crypto-lib type))

(define XIDEN_AVAILABLE_CHF_COUNT
  (get-ffi-obj* #"XIDEN_AVAILABLE_CHF_COUNT"
                _uint))


(define XIDEN_DEFAULT_CHF_INDEX
  (get-ffi-obj* #"XIDEN_DEFAULT_CHF_INDEX" _uint))


(define XIDEN_SUPPORTED_CHFS
  (get-ffi-obj* #"XIDEN_SUPPORTED_CHFS"
               (_array _string XIDEN_AVAILABLE_CHF_COUNT)))


(define load-chf/unsafe
  (get-ffi-obj* #"xiden_load_chf"
               (_fun _uint --> _EVP_MD-pointer)))


(define get-digest-size/unsafe
  (get-ffi-obj* #"xiden_get_digest_size"
               (_fun _EVP_MD-pointer --> _int)))


(define make-digest/unsafe!
  (get-ffi-obj* #"xiden_make_digest"
               (_fun _EVP_MD-pointer
                     _bytes
                     (_fun (_cpointer _int)
                           _uint
                           --> _gcpointer)
                     --> _int)))

(define start-signature/unsafe!
  (get-ffi-obj* #"xiden_start_signature"
                (_fun _pointer ; EVP_MD* p_md,
                      _pointer ; EVP_MD_CTX* p_md_ctx,
                      _bytes/nul-terminated  ; char* p_private_key_content,
                      _bytes/nul-terminated  ; char* p_private_key_password,
                      _bytes/nul-terminated ; char* p_digest,
                      _size    ; size_t digest_length
                      --> _int)))

(define find-signature-size/unsafe!
  (get-ffi-obj* #"xiden_find_signature_size"
                (_fun _pointer ; EVP_MD_CTX* p_md_ctx
                      --> _size)))

(define end-signature/unsafe!
  (get-ffi-obj* #"xiden_end_signature"
                (_fun _pointer ; EVP_MD_CTX* p_ctx
                      _pointer ; char* p_signature
                      _uint    ; size_t signature_length
                      --> _int)))

(define verify-signature/unsafe!
  (get-ffi-obj* #"xiden_verify_signature"
                (_fun _pointer ; EVP_MD* md,
                      _bytes/nul-terminated ; char* pSignature,
                      _uint    ; size_t signatureLength
                      _bytes/nul-terminated ; char* pPublicKeyContent,
                      _bytes/nul-terminated ; char* pDigest,
                      _uint    ; size_t digestLength
                      --> _int)))

(define make-md-context/unsafe
  (get-ffi-obj* #"EVP_MD_CTX_new"
                (_fun --> _pointer)))

(define get-next-libcrypt-error-code/unsafe
  (get-ffi-obj* #"ERR_get_error"
                (_fun --> _ulong)))

(define translate-libcrypt-error-code/unsafe
  (get-ffi-obj* #"ERR_error_string"
                (_fun _ulong _pointer --> _string)))

;-------------------------------------------------------------------------------
; User-friendly forms of foreign data

(define cryptographic-hash-functions
  (let* ([arr XIDEN_SUPPORTED_CHFS])
    (for/list ([i (in-range XIDEN_AVAILABLE_CHF_COUNT)])
      (string->symbol (string-downcase (array-ref arr i))))))


(define DEFAULT_CHF
  (list-ref cryptographic-hash-functions
            XIDEN_DEFAULT_CHF_INDEX))


(define (chf/c v)
  (and (member v cryptographic-hash-functions) #t))

(define (load-chf chf)
  (define mdindex (index-of cryptographic-hash-functions chf))
  (define p_md (load-chf/unsafe mdindex))
  (unless p_md (raise (raise-crypto-failure)))
  p_md)


(define (dump-error-queue [q null])
  (define code (get-next-libcrypt-error-code/unsafe))
  (if (zero? code)
      (reverse q)
      (dump-error-queue (cons code q))))


(define (get-crypto-error-strings $)
  (if ($crypto:error? $)
      (get-crypto-error-strings ($crypto:error-queue $))
      (map (let ([buffer (make-bytes 256)])
             (λ (n)
               (translate-libcrypt-error-code/unsafe n buffer)
               (bytes->string/utf-8 buffer)))
           $)))

(define (print-crypto-error $)
  (for ([m (in-list (get-crypto-error-strings $))])
    (displayln m)))


(define (make-digest variant [algorithm DEFAULT_CHF])
  (cond [(bytes? variant)
         (make-digest (open-input-bytes variant) algorithm)]
        [(path-string? variant)
         (call-with-input-file*
           variant
           (λ (from-file)
             (make-digest from-file algorithm)))]
        [else
         (define p-md (load-chf algorithm))
         (define digest-size (get-digest-size/unsafe p-md))
         (define read-buffer-size (* 128 1024))
         (define gc-buffer (make-bytes read-buffer-size))
         (define digest-buffer (make-bytes digest-size))

         (define (read-more p-size read-so-far)
           (define bytes-read
             (read-bytes! gc-buffer
                          variant
                          0
                          read-buffer-size))
           (and (not (eof-object? bytes-read))
                (begin (ptr-set! p-size _uint bytes-read)
                       gc-buffer)))

         (define status
           (make-digest/unsafe! p-md digest-buffer read-more))

         (if (equal? status 1)
             digest-buffer
             (raise-crypto-failure))]))


(define (raise-crypto-failure)
  (raise ($crypto:error (dump-error-queue))))


(define (verify-signature digest algorithm signature-bytes public-key-bytes)
  (define p-md (load-chf algorithm))
  (define digest-size (get-digest-size/unsafe p-md))

  (define status
    (verify-signature/unsafe! p-md
                              signature-bytes
                              (bytes-length signature-bytes)
                              public-key-bytes
                              digest
                              digest-size))

  (if (< status 0)
      (raise-crypto-failure)
      (equal? status 1)))


(define (make-signature digest algorithm private-key-bytes [private-key-password-bytes #f])
  (define p-md (load-chf algorithm))
  (define digest-size (get-digest-size/unsafe p-md))
  (define p-ctx (make-md-context/unsafe))

  ; Create new context
  (start-signature/unsafe! p-md
                           p-ctx
                           private-key-bytes
                           private-key-password-bytes
                           digest
                           digest-size)

  ; Allocate memory and read finished signature
  (define signature-size
    (find-signature-size/unsafe! p-ctx))

  (unless (> signature-size 0)
    (raise-crypto-failure))

  (define signature
    (make-bytes signature-size))

  (end-signature/unsafe! p-ctx
                         signature
                         signature-size)

  signature)


(define (sign-with-snake-oil digest [chf DEFAULT_CHF])
  (make-signature digest
                  chf
                  snake-oil-private-key
                  snake-oil-private-key-password))


(module+ test
  (require rackunit
           racket/file
           "codec.rkt")

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
    (with-handlers ([$crypto:error?
                     (λ ($)
                       (print-crypto-error $)
                       (fail))])
      (test-equal? (format "Create digest using CHF ~a" chf)
                   (make-digest data chf)
                   expected-digest)

      (define signature
        (sign-with-snake-oil expected-digest
                             chf))

      (define (valid? #:expected-digest [e expected-digest]
                      #:chf [c chf]
                      #:signature [s signature]
                      #:public-key [p snake-oil-public-key])
        (verify-signature e c s p))

      (define (tamper bstr)
        (define copy (bytes-copy bstr))
        (bytes-set! copy 0 (modulo (add1 (bytes-ref bstr 0)) 255))
        copy)

      (test-true "Create and verify signature"
                 (valid?))

      (test-false "Reject signatures with the wrong CHF"
                  (valid? #:chf (findf (λ (v) (not (equal? v chf)))
                                       cryptographic-hash-functions)))

      (test-false "Reject doctored signatures"
                  (valid? #:signature (tamper signature)))

      (test-false "Reject signatures against the wrong digest"
                  (valid? #:expected-digest (tamper expected-digest))))))
