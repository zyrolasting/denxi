#lang racket/base

; Useful for finding paths in expression position.
(define (get-path-if-file-exists f)
  (and (file-exists? f) f))
