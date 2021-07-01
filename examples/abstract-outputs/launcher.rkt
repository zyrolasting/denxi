#lang xiden/launcher
(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)
(XIDEN_WORKSPACE (build-path (current-directory) "workspace"))
(module+ main (launch-xiden!))

(module+ test
  (require (submod xiden/cli test))
  ; Don't bother verifying the second, because the database would not
  ; exist for it in a new workspace.
  (define-runtime-path install-first.rkt "install-first.rkt")
  (functional-test/install-all install-first.rkt))
