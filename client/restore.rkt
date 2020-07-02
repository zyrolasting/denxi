#lang racket/base

(provide restore-command)

(require racket/cmdline
         "../capture.rkt")

(define (restore-command)
  (command-line #:program "restore"
                #:args (path) (restore-workspace path)))
