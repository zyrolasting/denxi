#lang racket/base

; Use the metaphor of an office manager coordinating workers to
; perform jobs in paralell and handle errant conditions.

(provide process-jobs!)

(require racket/format
         racket/match
         racket/runtime-path
         racket/place
         "messages.rkt"
         "job.rkt"
         "worker.rkt"
         "../logging.rkt")

(struct manager (workers inbox orders))

(define (process-jobs! . initial-job-data)
  (iterate! (apply make-workplace! initial-job-data)))

; Assign jobs, start said jobs, and figure out what to do based on job progress.
(define (iterate! boss)
  (define with-new-orders   (make-orders boss))
  (define with-work-started (start-orders! boss))
  (define workers (manager-workers with-work-started))
  (if (all-jobs-done? (manager-inbox with-work-started))
      (dismiss-workers! workers)
      (apply sync/enable-break
             (for/list ([w (in-vector workers)])
               (make-worker-evt with-work-started w)))))


; Create a manager with workers and obligations.
; Side-effect: Racket places start up that must be
; freed with (dismiss-workers). (iterate!) does that.
(define (make-workplace! . initial-job-data)
  (define workers (make-workers! (length initial-job-data)))
  (define headcount (vector-length workers))
  (manager workers
           (apply add-jobs
                  (make-job-pool headcount
                                 initial-job-data))
           null))

; I execute all orders as a side-effect so that I
; can drop the references all at once. This mitigates
; the risk of starting redundent work, and makes it
; easier to test the code that makes the orders.
(define (start-orders! boss)
  (for ([assign! (manager-orders boss)])
    (assign!))
  (manager (manager-workers boss)
           (manager-inbox boss)
           null))

; Create the event where a worker has a report for the boss.
(define (make-worker-evt boss reporter)
  (handle-evt (worker-pch reporter)
              (λ (maybe-$report)
                (handle-$report! boss
                                 reporter
                                 (coerce-$report maybe-$report)))))


; Log the report, and use it to update the manager's understanding of the world.
(define (handle-$report! boss reporter report)
  (log-$report report)
  (iterate!
   (manager (record-last-seen (manager-workers boss)
                              reporter))
            (record-discovered-work (manager-inbox boss)
                                    ($report-data report))
            (manager-orders boss)))


; Use the contents of a worker's report to identify and schedule new work.
; If a boss tells a worker to build a desk, and a worker responds with
; a request for materials, then that request for materials is added here.
(define (record-discovered-work inbox data)
  (match data
    [($blocker blocked-job-id command)
     (add-blocking-job inbox blocked-job-id command)]
    [($progress job-id progress)
     (set-job-progress inbox job-id progress)]
    [_ inbox]))


; Allows a worker to send information on its place channel more freely.
(define (coerce-$report maybe-report)
  (if ($report? maybe-report)
      maybe-report
      ($report (~v maybe-report)
               'debug
               maybe-report)))


; Prepare orders to send new jobs to workers.
; Has no side-effects for ease of testing.
(define (make-orders boss)
  (define workers  (manager-workers boss))
  (define jobs     (manager-inbox boss))
  (define assigned (get-next-job jobs))
  (if assigned
      (make-orders
       (manager
        workers
        (set-job-progress jobs assigned 0) ; 0 = "0% progress". Semantically different from 'new.
        (cons (make-order workers assigned)
              (manager-orders boss))))
      boss))


; Create a procedure that sends an assigned job to a worker.
(define (make-order workers assigned)
  (λ ()
    (define jid (job-id assigned))
    (define wid (job-assignee assigned))
    (log-zcpkg-info "Starting jobs #~v on worker ~v" jid wid)
    (place-channel-put (worker-pch (vector-ref workers wid)
                                   ($start-job jid (job-data assigned))))))
