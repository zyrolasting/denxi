#lang racket/base

(require "default/symlinked.rkt"
         (only-in (symlinked "default/v1.rkt") my-struct?)
         (only-in (symlinked "default/v2.rkt") my-struct))

(displayln (if (my-struct? (my-struct))
               'same
               'different))
