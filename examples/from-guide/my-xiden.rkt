#lang xiden/launcher

; This is unsafe, and only meant for the demo.  We'll remove these in
; the next example. In effect, this disables all of Xiden's data
; verification features.

(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)

(module+ main (launch-xiden!))
