#lang denxi/launcher

(current-chfs (list snake-oil-chf))
(DENXI_FETCH_TOTAL_SIZE_MB +inf.0)
(DENXI_TRUST_BAD_DIGEST #t)

(module+ main
  (launch-denxi!))
