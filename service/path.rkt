#lang racket/base

(provide (all-defined-out))

(require racket/runtime-path)

(define-runtime-path catalogs/ "catalogs/")

(define public-key-file-name "key.pub")
