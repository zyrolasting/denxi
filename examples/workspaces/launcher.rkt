#lang denxi/launcher
(module+ main (launch-denxi!))

(current-chfs (list snake-oil-chf))
(DENXI_TRUST_BAD_DIGEST #t)

; Tell Denxi to perform transactions against a fixed, adjacent
; directory.  After running this launcher against the definition,
; you'll see a `workspace` directory appear with state information.
;
; Links are issued from there.

(require racket/runtime-path)
(define-runtime-path workspace "workspace")
(DENXI_WORKSPACE workspace)
