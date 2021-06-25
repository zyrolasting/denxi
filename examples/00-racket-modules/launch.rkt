#lang xiden/launcher

(XIDEN_TRUST_BAD_DIGEST #t)

(module+ main (call-with-snake-oil-chf-trust launch-xiden!))
