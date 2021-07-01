#lang xiden/launcher
(module+ main (launch-xiden!))

(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)

; Tell Xiden to perform transactions against a fixed, adjacent
; directory.  After running this launcher against the definition,
; you'll see a `workspace` directory appear with state information.
;
; Links are issued from there.

(require racket/runtime-path)
(define-runtime-path workspace "workspace")
(XIDEN_WORKSPACE workspace)
