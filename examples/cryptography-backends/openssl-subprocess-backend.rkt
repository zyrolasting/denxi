#lang xiden/launcher



(module+ main (launch-xiden!))

(require racket/function)

; Override Xiden's cryptographic operations
; in a zero-trust context.
;
; Instead of using built-in implementations, use the host's OpenSSL
; binary. We'll consent whenever the runtime asks to run it.

(define openssl
  (or (find-executable-path "openssl")
      (raise-user-error "openssl must be installed and be visible in your PATH")))

(define openssl-exec (curry system* openssl))

(define-syntax-rule (capture v)
  (parameterize ([current-output-port (open-output-bytes)])
    v
    (get-output-bytes (current-output-port) #t)))
