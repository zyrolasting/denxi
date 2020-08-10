#lang racket/base

(provide (all-defined-out))
(require "../workspace.rkt"
         scribble/manual)

(define wsdir (tt CONVENTIONAL_WORKSPACE_NAME))
(define depdir (tt CONVENTIONAL_DEPENDENCY_DIRECTORY_NAME))
(define binary (tt "zcpkg"))
(define definition (tt CONVENTIONAL_PACKAGE_INFO_FILE_NAME))

(define (tech/reference tag)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") tag))
