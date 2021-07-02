#lang xiden/launcher

(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)
(XIDEN_WORKSPACE (build-path (current-directory) "workspace"))

(module+ main (launch-xiden!))
