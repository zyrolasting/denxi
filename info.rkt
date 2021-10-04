#lang info

(define collection "denxi")

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
  '("docs"
    "crypto/openssl"
    "crypto/dist"))

(define compile-omit-paths
  '("examples"
    "crypto/openssl"
    "crypto/dist"))

(define racket-launcher-names '("denxi"))
(define racket-launcher-libraries '("cli.rkt"))

(define scribblings
  '(("docs/index/denxi-index.scrbl" () (tool))
    ("docs/reference/denxi-reference.scrbl" (multi-page) (tool-library))
    ("docs/guide/denxi-guide.scrbl" () (tool))
    ("docs/white-paper/denxi-white-paper.scrbl" () (other))))
