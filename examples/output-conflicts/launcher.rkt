#lang denxi/launcher
(module+ main (launch-denxi!))
(current-chfs (list snake-oil-chf))
(DENXI_WORKSPACE (build-path (current-directory) "workspace"))
(DENXI_TRUST_BAD_DIGEST #t)
