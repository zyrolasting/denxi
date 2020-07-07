#lang racket/base

; Define messages to send on place channels.

(provide (all-defined-out))

(require racket/place
         "../logging.rkt")

; Start a job. Use the ID for logging.
(struct $run (id command) #:prefab)

; Ask the user a question.
(struct $ask (prompt) #:prefab)

; End a job.
(struct $fin (id) #:prefab)

; Report commands that must finish in order.
(struct $seq (run-first run-after) #:prefab)


; The below bindings are for use in a created place.

(define current-place-channel (make-parameter #f))
(define current-job-id (make-parameter #f))

(define (send-upstream val)
  (place-channel-put (current-place-channel) val))

(define (<<fin)
  (send-upstream ($fin (current-job-id))))

(define (<<ask prompt)
  (send-upstream ($ask prompt))
  (read-line))

(define (<<seq run-first run-after)
  (send-upstream ($seq run-first run-after)))
