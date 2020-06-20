#lang racket/base

(require "../lib.rkt")

(module+ test
  (test-require-spec "publisher/pkg/other.rkt")
  (test-require-spec "big-publisher/std/lib.rkt")
  (test-require-spec/missing "publisher/pkg/main.rkt"))
