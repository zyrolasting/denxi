#lang racket/base

(provide uninstall-command)

(require racket/cmdline
         "../jobs/jobs.rkt"
         "../config.rkt")

(define (uninstall-command)
  (command-line #:args dependency-strings
                (apply start-work
                       (for/list ([path (in-list dependency-strings)])
                         (vector "uninstall" (if (complete-path? path)
                                                 path
                                                 (build-path (ZCPKG_INSTALL_RELATIVE_PATH)
                                                             path)))))))
