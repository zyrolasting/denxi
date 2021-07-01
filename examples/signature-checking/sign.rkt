#lang racket

; Bonus: This program creates a signature for you using your own
; private key. Using this, try to make your own keypair for the
; example.

(module+ main
  (require (only-in xiden/codec
                    hex)
           (only-in xiden/integrity
                    current-chfs
                    chf-canonical-name
                    snake-oil-chf)
           (only-in xiden/signature
                    make-signature)
           (only-in xiden/signature/ffi
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
        (~a "Sorry, but this system could not load Xiden's built-in crypto library.\n"
            "You can still do the bonus for this example using a custom\n"
            "cryptographic backend. See the related example!")))))
