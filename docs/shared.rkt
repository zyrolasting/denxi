#lang racket

(provide (all-defined-out))

(require "../workspace.rkt"
         scribble/manual
         syntax/strip-context)

(define wsdir (tt CONVENTIONAL_WORKSPACE_NAME))
(define binary (tt "xiden"))
(define project-name "Xiden")

(define (visible-hyperlink s)
  (hyperlink s s))

(define (reformat-syntax stx v)
  (replace-context stx
    (read-syntax #f (open-input-string (with-output-to-string (λ () (pretty-write v)))))))

(define (tech/reference tag)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") tag))
