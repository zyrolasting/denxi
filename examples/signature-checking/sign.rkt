#lang racket

; Bonus: This program creates a signature for you using your own
; private key. Using this, try to make your own keypair for the
; example.

(module+ main
  (require (only-in denxi/codec
                    hex)
           (only-in denxi/integrity
                    current-chfs
                    chf-canonical-name
                    snake-oil-chf)
           (only-in denxi/signature
                    make-signature)
           (only-in denxi/signature/ffi
                    signature-ffi-available?!))

  (current-chfs
   (list snake-oil-chf))

  (command-line
   #:args (private-key-path)
   (if (signature-ffi-available?!)
       (call-with-output-file "signature.bin"
         (λ (out)
           (define sig
             (make-signature (hex "59c826fc854197cbd4d1083bce8fc00d0761e8b3")
                             (chf-canonical-name snake-oil-chf)
                             (file->bytes private-key-path)
                             #f))
           (copy-port (open-input-bytes sig) out)))
       (displayln
        (~a "Sorry, but this system could not load Denxi's built-in crypto library.\n"
            "You can still do the bonus for this example using a custom\n"
            "cryptographic backend. See the related example!")))))
