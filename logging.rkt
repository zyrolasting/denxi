#lang racket/base

(provide (all-defined-out))

(require racket/logging)

(define-logger zcpkg)

(define-syntax-rule (show-zcpkg-logs body ...)
  (with-logging-to-port (current-output-port)
    (λ () body ...)
    #:logger zcpkg-logger
    'debug
    'zcpkg))
