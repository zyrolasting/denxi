#lang xiden/launcher

; Don't use these settings in production.
(current-chfs (list snake-oil-chf))
(XIDEN_TRUST_BAD_DIGEST #t)

(module+ main
  (launch-xiden!))

; This wasn't in the guide. It's a functional test. It does what you
; would do at the command line to help us verify examples.
(module+ test
  (require (submod xiden/cli test))
  (define-runtime-path definition.rkt "definition.rkt")
  (functional-test/install-all definition.rkt))
