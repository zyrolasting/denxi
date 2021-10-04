#lang denxi/launcher

; This version sets the workspace based on the current directory.  Try
; going to different directories to run installatons.
(module+ main (launch-denxi!))

(DENXI_WORKSPACE (build-path (current-directory) "following"))
(current-chfs (list snake-oil-chf))
(DENXI_TRUST_BAD_DIGEST #t)
