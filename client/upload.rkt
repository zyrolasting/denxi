#lang racket/base

(provide upload-command)

(require racket/cmdline)

(define (upload-command)
  (command-line #:program "upload"
                #:args () (void)))
