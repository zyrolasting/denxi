#lang racket/base

(require racket/file)

(define (set-up!)
  (eprintf "TRYING~n")
  (display-to-file "INSIDE" "here"))
