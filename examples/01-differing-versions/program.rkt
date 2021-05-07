#lang racket/base

(require "example01-output/symlinked.rkt"
         (only-in (symlinked "example01-output/v1.rkt") my-struct?)
         (only-in (symlinked "example01-output/v2.rkt") my-struct))

(displayln (if (my-struct? (my-struct))
               'same
               'different))
