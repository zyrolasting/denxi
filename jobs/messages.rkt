#lang racket/base

; Define messages to send on place channels.

(provide (all-defined-out))

(require racket/place)

; Send this to a worker to have it do a job.
; The ID is used to report progress.
(struct $start-job (id command) #:prefab)

; An envelope for messages from worker to manager.
; `data` should hold $progress or $blocker.
(struct $report (message level data) #:prefab)

; Send this to a manager to report progress on a job
(struct $progress (job-id scalar) #:prefab)

; Send this to a manager to report a command
; that blocks a given job.
(struct $blocker (job-id command) #:prefab)

; These are meant to be used in worker places.
(define current-output-pch (make-parameter #f))
(define current-job-id (make-parameter #f))
(define current-command (make-parameter #f))

(define-syntax-rule (dump-trace)
  (continuation-mark-set->context (current-continuation-marks)))

(define (send-report message
                     level
                     [data (dump-trace)])
  (place-channel-put (current-output-pch)
                     ($report message level data)))

(define (<< #:level [level 'info] #:data [data (dump-trace)] fmt-string . args)
  (send-report (apply format fmt-string args)
               level
               data))

(define (<<progress progress
                    [message (format "[~a%]: ~a"
                                     (if (eq? progress 'done)
                                         "100"
                                         (floor * 100 progress))
                                     (current-command))])
  (<< #:level 'info
      #:data ($progress (current-job-id) progress)
      message))

(define (<<blocker command)
  (<< #:level 'debug
      #:data ($blocker (current-job-id) command)
      "Job ~v depends on discovered command: ~v"
      (current-job-id)
      command))
