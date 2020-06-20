#lang racket/base

(provide (all-defined-out))

(require racket/logging
         "jobs/messages.rkt")

(define-logger zcpkg)

(define (log-$report id m)
  (log-message zcpkg-logger
               ($report-level m)
               (format "~a: ~a" id ($report-message m))
               ($report-data m)))
