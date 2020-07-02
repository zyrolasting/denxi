#lang racket/base

; Define a means to print values with job-based tracing information.

(provide (all-defined-out))

(define current-job-id (make-parameter #f))

(define (format/job fmt-string . args)
  (define msg (apply format fmt-string args))
  (if (current-job-id)
      (format "job ~a: ~a"
              (current-job-id)
              msg)
      msg))

(define (<< fmt-string . args)
  (write (apply format/job fmt-string args))
  (flush-output))
