#lang xiden/launcher

; To complete Step 2, add the name of the crypto hash function Xiden
; mentioned in Step 1 to this list. It should look like '(sha384),
; not '("sha384").
(XIDEN_TRUST_MESSAGE_DIGEST_ALGORITHMS '(sha384))

; Paste the expression from Step 4 into this list. This is how you
; tell Xiden that you trust a public key. Note that this list is not
; quoted!
(XIDEN_TRUST_PUBLIC_KEYS (list (integrity 'sha384 (base64 "2SXspwxa36HXciz2wftmftPnlncVpOrs9S40JmP4gjHeOflik+cZonrePYdmauVL"))))

(module+ main (launch-xiden!))
