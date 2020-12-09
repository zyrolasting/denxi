#lang info

(define collection "xiden")

(define version "0.0") ; Don't use. This project defines its own scheme.

(define deps '("base" "rackunit-lib" "db-lib"))

(define build-deps '("scribble-lib"))

(define racket-launcher-names '("xiden"))
(define racket-launcher-libraries '("cli.rkt"))

(define scribblings
  '(("docs/reference/xiden-reference.scrbl" (multi-page) (tool-library))
    ("docs/maintenance/xiden-maintenance.scrbl" (multi-page))
    ("docs/guide/xiden-guide.scrbl" (multi-page) (tool))))

(define raco-commands
  '(("zcpkg"
     (submod xiden/cli main)
     "manage dependencies without side-effects on the installation"
     70)))
