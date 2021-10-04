#lang denxi/launcher

(current-chfs (list snake-oil-chf))
(DENXI_FETCH_TOTAL_SIZE_MB +inf.0)
(DENXI_TRUST_BAD_DIGEST #t)
(DENXI_WORKSPACE (build-path (current-directory) "workspace"))

(module+ main
  (launch-denxi!))
