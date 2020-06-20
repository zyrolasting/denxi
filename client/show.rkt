#lang racket/base

(provide show-command)

(require racket/cmdline)


(define (show-command)
  (command-line
   #:args zcp-package-names
   (for ([versionless-name (in-list zcp-package-names)])
     (define info (zcpkg-get-info )))))
