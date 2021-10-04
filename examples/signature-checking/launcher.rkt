#lang denxi/launcher

(module+ main (launch-denxi!))

; This launcher checks integrity and signatures,
; but uses an untrustworthy CHF and public key.
; You can use this as a template to substitute
; public keys and CHFs you actually trust.

(require racket/runtime-path)
(define-runtime-path public-key "public-key.pem")

(current-chfs
 (list snake-oil-chf))

(DENXI_TRUST_PUBLIC_KEYS
 (list (make-trusted-integrity public-key)))

(DENXI_WORKSPACE
 (build-path (current-directory) "workspace"))
