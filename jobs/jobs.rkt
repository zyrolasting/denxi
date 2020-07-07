#lang racket/base

; Execute commands in parallel, such that every command is performed
; exactly once.

(provide start-work)

(require racket/format
         racket/future
         racket/list
         racket/match
         racket/runtime-path
         racket/place
         place-controller
         "messages.rkt"
         "../logging.rkt")

(define-runtime-path place.rkt "place.rkt")

(struct job (id state dependencies command) #:transparent)


(define (start-work . commands)
  ; Use a hash keyed on commands to prevent
  ; duplicate work. Jobs will still have
  ; ids and dependency info for logging
  ; and scheduling purposes.
  (define jobs
    (foldl (λ (cmd table)
             (add-job table cmd null))
           (hash)
           commands))
  (define job-count (hash-count jobs))
  (define team (make-team job-count))
  (<< "Starting ~v jobs" job-count)
  (process-jobs team jobs))


(define (process-jobs team jobs)
  (if (for/and ([(cmd j) (in-hash jobs)]) (job-done? j))
      (shut-down-workers team)
      (let ([remaining (send-jobs team jobs)])
        (match (apply sync (for/list ([$ (in-hash-values team)]) ($)))
          [(place-detach-envelope $)
           (<< #:worker (place-resources-id $)
               "Unexpected shutdown in worker. Stop the presses!")
           (process-jobs team (hash))]
          [(or (from-stdout $ v)
               (from-stderr $ v))
           (<< #:worker (place-resources-id $) v)
           (process-jobs team (if (eof-object? v) (hash) remaining))]
          [(from-place-channel $ v)
           (handle-place-message $ v team jobs)]
          [_ (process-jobs team remaining)]))))


(define (shut-down-workers team)
  (for ([$ (in-hash-values team)])
    ($ 'stop)
    (let loop ()
      (or (place-detach-envelope? (sync ($)))
          (loop)))))


(define (handle-place-message $ v team jobs)
  (match v
    [($seq run-first run-after)
     (process-jobs team (add-jobs jobs run-first run-after))]
    [($ask prompt)
     (display prompt)
     (flush-output)
     ($ (λ (o) (displayln (read-line) o)))
     (process-jobs team jobs)]
    [($fin id)
     (<< #:level 'debug
         #:worker (place-resources-id $)
         #:job id
         "finished job")
     (process-jobs team (finish-job jobs id))]))


(define (make-team job-count)
  (define num (min (processor-count) job-count))
  (<< "Spawning ~v workers" num)
  (for/hash ([id (in-range num)])
    (values id (make-place-controller/dynamic-place* #:id id place.rkt))))


(define (send-jobs team jobs)
  (for/hash ([(cmd j) (in-hash jobs)])
    (values cmd
            (if (job-ready? j)
                (assign-job team j)
                j))))


(define (finish-job jobs finished-job-id)
  (for/hash ([(cmd j) (in-hash jobs)])
    (values cmd
            (if (eq? (job-id j) finished-job-id)
                (struct-copy job j
                             [state 'done])
                (struct-copy job j
                             [dependencies
                              (remove finished-job-id
                                      (job-dependencies j))])))))


(define (assign-job team j)
  ; Simple round robin
  (define assignee (modulo (job-id j) (hash-count team)))
  (define place-controller (hash-ref team assignee))
  (define message ($run (job-id j) (job-command j)))
  (<< #:level 'debug
      "Sending job ~a to worker ~a"
      (job-id j)
      assignee)
  (place-controller message)
  (struct-copy job j [state 'sent]))


(define (job-ready? j)
  (and (null? (job-dependencies j))
       (eq? (job-state j) 'new)))

(define (job-done? j)
  (eq? (job-state j) 'done))

(define (add-jobs jobs dependencies dependent)
  (add-job
   (for/fold ([updated jobs])
             ([cmd (in-list dependencies)])
     (add-job updated cmd null))
   dependent
   dependencies))

(define (add-job table command [dependencies null])
  (if (hash-has-key? table command)
      table
      (hash-set table
                command
                (job (hash-count table)
                     'new dependencies command))))
