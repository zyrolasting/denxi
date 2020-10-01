#lang info

(define collection "xiden")

(define version "0.0") ; Don't use. This project defines its own scheme.

(define deps '("base" "rackunit-lib" "db-lib" "base32"))

(define scribblings
  '(("docs/public-reference/xiden-public-reference.scrbl" (multi-page) (tool-library))
    ("docs/private-reference/xiden-private-reference.scrbl" (multi-page))
    ("docs/guide/xiden-guide.scrbl" (multi-page) (tool))))

(define raco-commands
  '(("zcpkg"
     (submod xiden/cli main)
     "manage dependencies without side-effects on the installation"
     70)))
