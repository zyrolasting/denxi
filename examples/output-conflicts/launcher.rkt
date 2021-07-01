#lang xiden/launcher
(module+ main (launch-xiden!))
(current-chfs (list snake-oil-chf))
(XIDEN_WORKSPACE (build-path (current-directory) "workspace"))
(XIDEN_TRUST_BAD_DIGEST #t)
