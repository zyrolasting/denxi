#lang denxi/launcher

; Trust any `sh' executable that can be found in the host's PATH.
(DENXI_TRUST_HOST_EXECUTABLES '("sh"))

(DENXI_TRUST_UNSIGNED #t)

(module+ main (launch-denxi!))
