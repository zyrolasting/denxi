#lang xiden/launcher

(require racket/runtime-path)

(define-runtime-path workspace "workspace")

(XIDEN_TRUST_BAD_DIGEST #t)
(XIDEN_WORKSPACE (path->complete-path workspace))

(current-string->source file-source)

(module+ main (launch-xiden!))
