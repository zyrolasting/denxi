#lang racket/base

; Define an entry point for parallel work, so that the CLI can
; distribute tasks around the user's hardware.  Use a metaphor to help
; the reader understand how this module manages Racket places.

(provide process-jobs
         make-company
         stop-company)

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
         "message.rkt"
         "prompt.rkt"
         "string.rkt")

; Company metaphor: A company has workers (places) and jobs to do (messages).
; The message pump in this module sends jobs to workers.
(struct company (workers jobs)
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

; Define an entry point for parallel work, such that messages are
; built into the company metaphor.
(define (process-jobs team)
  (let loop ([wip team])
    (if (and (andmap worker-idle? (company-workers wip))
             (null? (company-jobs wip)))
        wip
        (loop (update-company wip)))))

(define (make-company messages)
  (company (start-workers (length messages)) messages))

(define (start-workers job-count)
  (for/list ([id (in-range (max 1 (min (sub1 (processor-count)) job-count)))])
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
  (match-define (company workers jobs) team)
  (define no-jobs? (null? jobs))
  (company (list-update workers id
                        (λ (w)
                          (unless no-jobs?
                            (w (car jobs)))
                          (struct-copy worker w
                                       [idle? no-jobs?])))
           (if no-jobs? null (cdr jobs))))

(define (on-new-dependencies team dependent-name dependencies)
  (define new-jobs
    (map $install-package dependencies))

  ; Guard against duplicate jobs
  (define deduped
    (set-union (apply set new-jobs)
               (company-jobs team)))

  (company (company-workers team)
           (append (set->list deduped)
                   (company-jobs team))))

(define (on-package-installed team info)
  (printf "Installed ~a~n" info)
  team)

(define-message-pump (handle-team-event company?)
  on-idle
  on-package-installed)
