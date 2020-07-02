#lang racket/base

(provide serve-command)

(require racket/cmdline
         "../url.rkt"
         "../service/endpoint.rkt"
         "../service/server.rkt")

(define (serve-command)
  (void (start-server))
  (printf "Service up at ~a~n^C to stop~n" (url->string (make-endpoint)))
  (with-handlers ([exn:break? (λ (e) (displayln "bye"))])
    (sync/enable-break never-evt)))
