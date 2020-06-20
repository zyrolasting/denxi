#lang racket/base

(provide main)

(require racket/file
         racket/match
         racket/place
         "../source.rkt"
         "../path.rkt"
         "messages.rkt")

(define (process-order order)
  (match order
    [($start-job id command)
     (current-job-id id)
     (current-command command)
     (process-order command)]
    [(vector "echo" v)
     (<<progress 'done (format "echo ~a" v))]
    [_ (<< "Unrecognized command: ~a" order)]))


(define (main pch)
  (with-handlers ([exn:break? void])
    (current-output-pch pch)
    (let loop ()
      (sync/enable-break
       (handle-evt pch
                   (λ (order)
                     (process-order order)
                     (loop)))))))
