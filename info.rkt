#lang info

(define collection "denxi")

(define deps
  '("base"
    "compatibility-lib"
    "rackunit-lib"
    "sandbox-lib"
    "scribble-lib"))

(define build-deps
  '("net-doc" "racket-doc"))

(define test-omit-paths
  '("docs"))

(define scribblings
  '(("docs/index/denxi-index.scrbl" () (tool))
    ("docs/reference/denxi-reference.scrbl" (multi-page) (tool-library))
    ("docs/guide/denxi-guide.scrbl" () (tool))
    ("docs/journal/denxi-journal.scrbl" (multi-page) (omit))
    ("docs/white-paper/denxi-white-paper.scrbl" () (omit))))
