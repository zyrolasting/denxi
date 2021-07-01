#lang xiden/launcher

; Trust any `sh' executable that can be found in the host's PATH.
(XIDEN_TRUST_HOST_EXECUTABLES '("sh"))

(XIDEN_TRUST_UNSIGNED #t)

(module+ main (launch-xiden!))
