#lang denxi/launcher

; To complete Step 2, add the name of the crypto hash function Denxi
; mentioned in Step 1 to this list. It should look like '(sha384),
; not '("sha384").
(DENXI_TRUST_CHFS '())

; Paste the expression from Step 4 into this list. This is how you
; tell Denxi that you trust a public key. Note that this list is not
; quoted!
(DENXI_TRUST_PUBLIC_KEYS (list))

(module+ main (launch-denxi!))
