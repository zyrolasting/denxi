#lang xiden

; Integrity information makes sure that an artifact's content is
; correct. This is especially important if the data comes from the
; network.

; Notice that we can express the digests using different encodings
; and names for SHA-1. The launcher shows how this works.

; If you mess with the integrity information OR the content, the
; installation will fail integerity checks. Try it!

(input "food"
  (artifact (byte-source #"cookie")
            (integrity 'SHA-1
                       (hex "59c826fc854197cbd4d1083bce8fc00d0761e8b3"))
            #f))

(input "drink"
  (artifact (byte-source #"milk")
            (integrity 'sha1
                       (base64 "z12/DsV9/12lbYbEWzvdEYSaBlo="))
            #f))

(output "default"
        (keep-input "food")
        (keep-input "drink"))
