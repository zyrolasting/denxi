#lang racket/base

(provide install-command)

(require racket/cmdline
         "../jobs/manager.rkt")

(define (install-command)
  (command-line #:args package-sources
                (apply process-jobs!
                       (for/list ([source (in-list package-sources)])
                         (vector "install" source)))))
