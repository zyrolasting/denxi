#lang denxi/launcher

(module+ main (launch-denxi!))

(DENXI_WORKSPACE (build-path (current-directory) "workspace"))

; Here we give a SHA-1 implementation to Denxi with a pattern to
; recognize variations in names (sha1, SHA-1, ...). `sha1-bytes` is
; not a good implementation for production, but it is compatible with
; every Racket installation.
(require file/sha1)
(current-chfs (list (chf 'sha1 #"^(?i:sha-_?1)$" sha1-bytes)))

; Previously we used no integrity information in our artifact, so we
; no longer shut off integrity checking using DENXI_TRUST_BAD_DIGEST.
; In practice, we should respond to missing integrity information
; by adding that information, NOT by shutting off safety checks!
;
; However, we are still missing a signature. We'll trust that
; scenario for now.
(DENXI_TRUST_UNSIGNED #t)
