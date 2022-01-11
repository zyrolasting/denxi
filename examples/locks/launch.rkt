#lang denxi/launcher

(DENXI_TRUST_BAD_DIGEST #t)

(current-string->source file-source)

(module+ main (launch-denxi!))
