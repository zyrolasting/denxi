#lang xiden

; Here's the definition from the guide. Let's dig a little deeper.


; Here's the sole input. Its (artifact) has three values:
;
;  - A source for content
;  - Integrity information to make sure the content is correct
;  - Signature information to make sure the content came from a trusted party.
;
; Here we only have the content, expressed as inline text. This makes
; it easy to trust for a demo.
;
; Run `raco docs xiden/artifact` for more information.

(input "hello.txt"
  (artifact (text-source "Hello, world!") #f #f))


; Here's the sole output.
;
; The "default" name is special in that you do not have to specify it
; in an installation using `do +a` or `do ++install-abbreviated`.
;
; (keep-input) looks up a defined input and creates a link on the file
; system pointing to the input's data.
;
; Keep leaning on `raco docs`, because I won't always repeat details
; from the reference.

(output "default"
  (keep-input "hello.txt"))
