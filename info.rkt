#lang info

(define collection "xiden")

(define version "0.0") ; Don't use. This project defines its own scheme.

(define deps '("base" "rackunit-lib" "db-lib" "base32"))

(define scribblings
  '(("docs/reference/xiden-reference.scrbl" (multi-page))
    ("docs/guide/xiden-guide.scrbl" (multi-page))))
