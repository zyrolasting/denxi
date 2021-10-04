#lang denxi/launcher

(DENXI_TRUST_BAD_DIGEST #t)
(DENXI_TRUST_HOST_EXECUTABLES '("raco"))

(module+ main (launch-denxi!))
