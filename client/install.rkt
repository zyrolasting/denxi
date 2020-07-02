#lang racket/base

(provide install-command)

(require racket/cmdline
         "../jobs/jobs.rkt")

(define (install-command)
  (command-line #:args package-sources
                (apply start-work
                       (for/list ([source (in-list package-sources)])
                         (vector "install" source)))))
