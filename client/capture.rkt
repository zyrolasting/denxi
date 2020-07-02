#lang racket/base

(provide capture-command)

(require racket/cmdline
         "../capture.rkt")

(define (capture-command)
  (command-line #:program "capture"
                #:args () (capture-workspace)))
