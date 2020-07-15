#lang racket/base

; Define an entry point for parallel work, so that the CLI can
; distribute tasks around the user's hardware.  Use a metaphor to help
; the reader understand how this module manages Racket places.

(provide process-jobs
         make-company
         stop-company
         (struct-out company))

(require racket/format
         racket/future
         racket/list
         racket/match
         racket/runtime-path
         racket/place
         racket/set
         racket/match
         (only-in "worker.rkt" worker-main)
         "config.rkt"
         "logging.rkt"
         "message-pump.rkt"
         "message.rkt"
         "prompt.rkt"
         "string.rkt")

; Company metaphor: A company has workers (places) and jobs to do
; (messages).  The message pump in this module sends jobs to workers.
; The backlog is the same as jobs, except they are accumulated for
; later processing. This is important for reprioritizing and
; sequencing work (e.g. downloading packages after knowing what all
; there is to download).
(struct company (workers jobs backlog)
  #:property prop:evt
  (λ (self) (apply choice-evt (company-workers self))))

; A worker is a place + the place's resources.
; Applying a worker is the same as sending a message to the corresponding place.
; Syncronizing on a worker is the same as waiting for a report from that worker.
(struct worker (id channel idle?)
  #:property prop:evt (struct-field-index channel)
  #:property prop:procedure
  (λ (self v)
    (place-channel-put (worker-channel self) v)
    self))

; Do all jobs in the company. Workers may discover work to put in the
; backlog. Once the jobs are done, new jobs should be selected from
; the backlog.
(define (process-jobs team)
  (let loop ([wip team])
    (if (and (andmap worker-idle? (company-workers wip))
             (null? (company-jobs wip)))
        team
        (loop (update-company wip)))))

(define (make-company messages)
  (company (start-workers)
           messages
           null))

(define (start-workers)
  (for/list ([id (in-range (processor-count))])
    (define pch (place inner-pch (worker-main inner-pch)))
    ; The worker needs to identify itself for some messages.
    (place-channel-put pch ($assign-id id))
    (worker id pch #f)))

(define (stop-company team)
  (for ([w (in-list (company-workers team))])
    (define ch (worker-channel w))
    (place-channel-put ch ($stop))
    (or (sync/timeout 0.5 (place-dead-evt ch))
        (place-kill ch))))

(define (update-company team)
  (let ([variant (sync team)])
    (cond [($message? variant)
           (handle-team-event team variant)]
          [(input-port? variant)
           (displayln (read-line variant))
           team]
          [else (write variant)
                team])))

;; Message handlers

; If a worker says it has nothing to do, then give it work.
(define (on-idle team id)
  (match-define (company workers jobs _) team)
  (define no-jobs? (null? jobs))
  (struct-copy company team
               [workers
                (list-update workers id
                        (λ (w)
                          (unless no-jobs?
                            (w (car jobs)))
                          (struct-copy worker w
                                       [idle? no-jobs?])))]
               [jobs
                (if no-jobs? null (cdr jobs))]))

(define (backlog-job team backlog? job)
  (struct-copy company team
               [backlog (cons job (company-backlog team))]))

(define (add-job team job)
  (struct-copy company team
               [jobs (cons job (company-jobs team))]))

(define-message-pump (handle-team-event company? default-message-handler)
  add-job
  backlog-job
  on-idle)
