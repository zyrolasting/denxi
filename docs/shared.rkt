#lang racket/base

(provide (all-defined-out))
(require "../workspace.rkt"
         scribble/manual)

(define wsdir (tt CONVENTIONAL_WORKSPACE_NAME))
(define binary (tt "xiden"))
(define project-name "Xiden")

(define (tech/reference tag)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") tag))
