#lang xiden/launcher

(XIDEN_TRUST_BAD_DIGEST #t)
(XIDEN_TRUST_HOST_EXECUTABLES '("raco"))

(module+ main (launch-xiden!))
