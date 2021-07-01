#lang xiden/launcher

(module+ main (launch-xiden!))

; Defeat the output cache to prevent conflicts.
; Helps you iterate in this example.
(current-package-editor sxs)
(XIDEN_WORKSPACE (build-path (current-directory) "workspace"))
(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)

(module+ test
  (require (submod xiden/cli test))
  (define-runtime-path defn.rkt "defn.rkt")
  (parameterize ([current-package-editor values])
    (XIDEN_ALLOW_UNSUPPORTED_RACKET
     #t (λ _ (functional-test/install-all defn.rkt)))))
