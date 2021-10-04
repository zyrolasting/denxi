#lang denxi/launcher

(module+ main (launch-denxi!))

; Defeat the output cache to prevent conflicts.
; Helps you iterate in this example.
(current-package-editor sxs)
(DENXI_WORKSPACE (build-path (current-directory) "workspace"))
(current-chfs (list snake-oil-chf))
(DENXI_TRUST_BAD_DIGEST #t)

(module+ test
  (require (submod denxi/cli test))
  (define-runtime-path defn.rkt "defn.rkt")
  (parameterize ([current-package-editor values])
    (DENXI_ALLOW_UNSUPPORTED_RACKET
     #t (λ _ (functional-test/install-all defn.rkt)))))
