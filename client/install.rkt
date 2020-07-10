#lang racket/base

(provide install-command)

(require racket/cmdline
         "../team.rkt"
         "../message.rkt")

(define (install-command)
  (command-line #:args package-sources
                (process-jobs
                 (for/list ([source (in-list package-sources)])
                   ($install-package source)))))
