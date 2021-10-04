#lang denxi/launcher

(require racket/runtime-path)

(define-runtime-path workspace "workspace")

(DENXI_TRUST_BAD_DIGEST #t)
(DENXI_WORKSPACE (path->complete-path workspace))

(current-string->source file-source)

(module+ main (launch-denxi!))
