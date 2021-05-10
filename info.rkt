#lang info

(define collection "xiden")

(define version "0.0") ; Don't use. This project defines its own scheme.

(define deps
  '("base"
    "compatibility-lib"
    "db-lib"
    "rackunit-lib"
    "sandbox-lib"
    "scribble-lib"))

(define build-deps '("net-doc" "racket-doc"))

(define test-omit-paths
  '("examples"
    "docs"))

(define compile-omit-paths
  '("examples"))

(define racket-launcher-names '("xiden"))
(define racket-launcher-libraries '("cli.rkt"))

(define scribblings
  '(("docs/index/xiden-index.scrbl" () (tool))
    ("docs/reference/xiden-reference.scrbl" (multi-page) (tool-library))
    ("docs/guide/xiden-guide.scrbl" (multi-page) (other))
    ("docs/practices/xiden-practices.scrbl" (multi-page) (other))
    ("docs/tutorials/xiden-tutorials.scrbl" (multi-page) (other))))
