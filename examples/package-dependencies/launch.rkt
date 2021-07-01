#lang xiden/launcher

(XIDEN_WORKSPACE (build-path (current-directory) "workspace"))
(XIDEN_TRUST_BAD_DIGEST #t)

(module+ main (launch-xiden!))
