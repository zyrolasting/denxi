#lang denxi/launcher

; Don't use these settings in production.
(current-chfs (list snake-oil-chf))
(DENXI_TRUST_BAD_DIGEST #t)

(module+ main
  (launch-denxi!))

; This wasn't in the guide. It's a functional test. It does what you
; would do at the command line to help us verify examples.
(module+ test
  (require (submod denxi/cli test))
  (define-runtime-path definition.rkt "definition.rkt")
  (functional-test/install-all definition.rkt))
