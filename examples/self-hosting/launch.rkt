#lang xiden/launcher

; Trust any `sh' executable that can be found in the host's PATH.
(XIDEN_TRUST_HOST_EXECUTABLES '("sh"))

; Trust the public key used to verify the signature on the inputs
(XIDEN_TRUST_PUBLIC_KEYS
 (list (integrity 'sha384 (base64 "2SXspwxa36HXciz2wftmftPnlncVpOrs9S40JmP4gjHeOflik+cZonrePYdmauVL"))))

; Trust the _exact_ Racket executable created by the input script.
(XIDEN_TRUST_EXECUTABLES
 (list (integrity 'sha384 (base64 "SxPU+tB4Pskzsjj9NT8L7a7S5iCXQWBk1ZGZgYCINXuQcFIxjLepin5FE+MzsAYE"))))

(XIDEN_TRUST_CHFS
 '(sha384))

(module+ main (launch-xiden!))
