#lang racket/base

(require "../lib.rkt")

(module+ test
  (test-require-spec "publisher/pkg/main.rkt")
  (test-require-spec "big-publisher/std/lib.rkt")
  (test-require-spec/missing "pkg/other.rkt"))
