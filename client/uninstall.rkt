#lang racket/base

(provide uninstall-command)

(require racket/cmdline
         "../team.rkt"
         "../message.rkt"
         "../config.rkt")

(define (uninstall-command)
  (command-line #:args dependency-strings
                (process-jobs
                 (for/list ([ds (in-list dependency-strings)])
                   ($uninstall-package ds)))))
