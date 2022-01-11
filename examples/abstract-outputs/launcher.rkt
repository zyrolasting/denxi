#lang denxi/launcher

(current-chfs (list snake-oil-chf))
(DENXI_TRUST_BAD_DIGEST #t)

(module+ main
  (launch-denxi!))

(module+ test
  (require (submod denxi/cli test))
  ; Don't bother verifying the second, because the database would not
  ; exist for it in a new workspace.
  (define-runtime-path install-first.rkt "install-first.rkt")
  (functional-test/install-all install-first.rkt))
