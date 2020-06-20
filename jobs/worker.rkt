#lang racket/base

(provide make-workers!
         dismiss-workers!
         record-last-seen
         (struct-out worker))

(require racket/future
         racket/runtime-path
         racket/place
         "messages.rkt")

(define-runtime-path place.rkt "place.rkt")

(struct worker (id pch last-seen))

; Use make-place in tests.
(define (make-workers! requested [make-place (λ () (dynamic-place place.rkt 'main))])
  (define worker-count (min requested (processor-count)))
  (for/vector ([id (in-range worker-count)])
    (worker id
            (make-place)
            (current-inexact-milliseconds))))


(define (dismiss-workers! workers)
  (for ([w (in-vector workers)])
    (define pch (worker-pch w))
    (place-break pch)
    (sync (place-dead-evt pch)
          (handle-evt
           (alarm-evt (+ (worker-last-seen w) 200)) ; Stragglers will be shot.
           (λ (alarm) (place-kill pch))))))


; Knowing when we last heard from a worker helps us
; decide if it needs to be kil-, er, fired.
(define (record-last-seen workers reporting-worker)
  (for/vector ([w (in-vector workers)])
    (if (eq? w reporting-worker)
        (worker (worker-id w)
                (worker-pch w)
                (current-inexact-milliseconds))
        w)))
