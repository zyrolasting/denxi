#lang xiden/launcher

; Don't use these settings in production.
; A later example will make that very clear.
(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)

(module+ main (launch-xiden!))
