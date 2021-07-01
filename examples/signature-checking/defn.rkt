#lang xiden

; Here we include signature information.  If you trust the public key,
; then you can perform a signature verification.

; The content, integrity information, and signature are mathematically
; linked. If you change the content, you change the integrity
; information. If you change the integrity information, you change the
; signature.
;
; How useful this is depends on the CHF and cipher you use.

(input "food"
       (artifact (byte-source #"cookie")
                 (integrity 'sha1
                            (hex "59c826fc854197cbd4d1083bce8fc00d0761e8b3"))
                 (signature (file-source (from-file "public-key.pem"))
                            (file-source (from-file "signature.bin")))))

(output "default"
  (keep-input "food"))
