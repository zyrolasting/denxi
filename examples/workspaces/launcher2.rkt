#lang xiden/launcher

; This version sets the workspace based on the current directory.  Try
; going to different directories to run installatons.
(module+ main (launch-xiden!))

(XIDEN_WORKSPACE (build-path (current-directory) "following"))
(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)
