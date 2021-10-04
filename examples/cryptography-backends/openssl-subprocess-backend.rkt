#lang denxi/launcher

(module+ main (launch-denxi!))

(require racket/file
         racket/function
         racket/random)
#|
This launcher sets Denxi's cryptographic operations to use the host's
OpenSSL binary. This affects EVERYTHING down to how Denxi stores state.

If you launch a REPL in this module, try these expressions.

 (define chf
   (get-default-chf))

 (define digest
   (make-digest #"something or other" chf))

 (define signature
   (make-signature digest
                   chf
                   snake-oil-private-key
                   snake-oil-private-key-password))

 (verify-signature digest
                   chf
                   snake-oil-public-key
                   signature)
|#

(define openssl
  (or (find-executable-path "openssl")
      (raise-user-error "openssl must be installed and be visible in your PATH")))


;-------------------------------------------------------------------------------
; Integrity checking

(define (make-chf name)
  (chf name
       (pregexp (~a name))
       (λ (in)
         (parameterize ([current-input-port in]
                        [current-output-port (open-output-bytes)])
           (system* openssl "dgst" (~a "-" name) "-binary")
           (get-output-bytes (current-output-port) #t)))))


;-------------------------------------------------------------------------------
; Signature checking

(define (verify-signature digest-bytes chf-name public-key-bytes signature-bytes)
  (define (start)
    (call-with-openssl-input-file
     #:scramble? #f
     "pub"
     public-key-bytes
     with-public-key))

  (define (with-public-key public-key-path)
    (call-with-openssl-input-file
     #:scramble? #f
     "sig"
     signature-bytes
     (curry verify public-key-path)))

  (define (verify public-key-path signature-path)
    (regexp-match?
     #rx#"Success"
     (parameterize ([current-input-port (open-input-bytes digest-bytes)]
                    [current-output-port (open-output-bytes)])
       (system* openssl
                "pkeyutl"
                "-verify"
                "-sigfile" signature-path
                "-pubin"
                "-inkey" public-key-path)
       (get-output-bytes (current-output-port) #t))))

  (start))


;-------------------------------------------------------------------------------
; Signature creation. Beware: Secrets in memory!

(define (make-signature digest-bytes chf-name private-key-bytes password-bytes)
  (define (start)
    (call-with-openssl-input-file
     #:scramble? #t
     "prv"
     private-key-bytes
     with-private-key))

  (define (with-private-key private-key-path)
    (call-with-openssl-input-file
     #:scramble? #t
     "pas"
     password-bytes
     (curry sign private-key-path)))

  (define (sign private-key-path password-path)
    (define base-args
      (list openssl
            "pkeyutl"
            "-sign"
            "-inkey" private-key-path))
    (parameterize ([current-input-port (open-input-bytes digest-bytes)]
                   [current-output-port (open-output-bytes)])
      (apply system* (if password-path
                         (append base-args
                                 (list "-passin" (format "file:~a" password-path)))
                         base-args))
      (get-output-bytes (current-output-port) #t)))
  (start))


;--------------------------------------------------------------------------------
; File I/O
;
; We put secrets in files so that `top`, `htop`, etc. won't leak the
; secret in a process list. The secret is kept on disk only long
; enough for use, then the content is scrambled on escaping the
; continuation.

(define temp-dir
  (build-path (find-system-path 'temp-dir)
              ".denxi-openssl"))

(define (call-with-openssl-input-file #:scramble? scramble? name byte-string f)
  (define path (build-path temp-dir name))
  (dynamic-wind
    (λ ()
      (make-directory* temp-dir)
      (call-with-output-file* path #:exists 'truncate/replace
        (λ (to-file)
          (write-bytes byte-string to-file))))
    (λ ()
      (f path))
    (λ ()
      ; Use crypto-random-bytes to refill the file with nonsense
      ; before deletion. Only does one pass, but the corrupted
      ; file length may differ from the original.
      (when scramble?
        (call-with-output-file* path #:exists 'truncate/replace
          (λ (to-file)
            (let loop ([remaining (bytes-length byte-string)])
              (if (<= remaining 0)
                  (void)
                  (begin (write-bytes (crypto-random-bytes 256) to-file)
                         (loop (- remaining 256))))))))
      (delete-file path))))


;-------------------------------------------------------------------------------
; Replace the back end

(current-chfs (list (make-chf 'sha256)))
(current-make-signature make-signature)
(current-verify-signature verify-signature)
