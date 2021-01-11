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

(define racket-launcher-names '("xiden"))
(define racket-launcher-libraries '("cli.rkt"))

(define compile-omit-paths
  '("examples"))

(define scribblings
  '(("docs/reference/xiden-reference.scrbl" (multi-page) (tool-library))
    ("docs/guide/xiden-guide.scrbl" (multi-page) (tool))))

(define raco-commands
  '(("zcpkg"
     (submod xiden/cli main)
     "manage dependencies without side-effects on the installation"
     70)))
