#lang racket/base

; Define jobs, job pools, and a scheduling method.

(provide make-job-pool
         get-next-job
         set-job-progress
         add-job
         add-jobs
         add-blocking-job
         job-ref
         all-jobs-done?
         (struct-out job)
         (struct-out job-pool))

(require racket/set)

(struct job (id assignee progress data blockers) #:transparent)
(struct job-pool (headcount table) #:transparent)

(define (add-job pool data)
  (define headcount (job-pool-headcount pool))
  (define table (job-pool-table pool))
  (define size (hash-count table))
  (define assignee (modulo size headcount)) ; Simple round robin
  (job-pool headcount
            (hash-set table size (job size assignee 'new data (seteq)))))

; Should not be part of the public interface, since this can easily
; create nonsense in the wrong hands.
(define (set-job pool id-or-job replace)
  (define table (job-pool-table pool))
  (define job-id (coerce-id id-or-job))
  (job-pool (job-pool-headcount pool)
            (hash-set table
                      job-id
                      (replace (hash-ref table job-id)))))

(define (add-blocker pool id-or-job blocking-id-or-job)
  (set-job pool id-or-job
           (λ (j) (struct-copy job j
                               [blockers
                                (set-add (job-blockers j)
                                         (coerce-id blocking-id-or-job))]))))

(define (add-blocking-job pool blocked-job-or-id command)
  (add-blocker (add-job pool command)
               (coerce-id blocked-job-or-id)
               (hash-count (job-pool-table pool)))) ; Using prior table to avoid sub1

(define (add-jobs pool data . args)
  (define next (add-job pool data))
  (if (null? args)
      next
      (apply add-jobs next args)))

(define (make-job-pool headcount)
  (job-pool headcount (hasheq)))

(define (get-next-job pool)
  (for/or ([(k v) (in-hash (job-pool-table pool))])
    (and (eq? (job-progress v) 'new)
         (set-empty? (job-blockers v))
         v)))

(define (job-ref pool job-id)
  (hash-ref (job-pool-table pool) job-id))

(define (set-job-progress pool id-or-job progress)
  (define id (coerce-id id-or-job))

  (define updated
    (set-job pool id
             (λ (j) (struct-copy job j
                                 [progress progress]))))

  ; If the job is done, then it no longer blocks other jobs.
  (if (eq? progress 'done)
      (job-pool (job-pool-headcount updated)
                (for/hash ([(k j) (in-hash (job-pool-table updated))])
                  (values k
                          (struct-copy job j
                                       [blockers
                                        (set-remove
                                         (job-blockers j) id)]))))
      updated))


(define (coerce-id id-or-job)
  (if (job? id-or-job)
      (job-id id-or-job)
      id-or-job))

(define (mark-jobs-done pool . ids-or-jobs)
  (if (null? ids-or-jobs)
      pool
      (apply mark-jobs-done
             (set-job-progress pool (car ids-or-jobs) 'done)
             (cdr ids-or-jobs))))

(define (all-jobs-done? pool)
  (for/and ([j (in-hash-values (job-pool-table pool))])
    (eq? 'done (job-progress j))))

(define (job-can-start? j)
  (and (eq? (job-progress j) 'new)
       (set-empty? (job-blockers j))))


(module+ test
  (require rackunit)

  (define 0-jobs (make-job-pool 2))
  (define 1-job  (add-job  0-jobs  'A))
  (define 2-jobs (add-job  1-job   'B))
  (define 3-jobs (add-job  2-jobs  'C))

  (test-equal? "Create a job pool"
               0-jobs
               (job-pool 2 (hasheq)))

  (test-equal? "Functionally add a job"
               1-job
               (job-pool 2 (hasheq 0 (job 0 0 'new 'A (seteq)))))

  (test-equal? "Functionally add jobs in bulk"
               (add-jobs 0-jobs 'A 'B 'C)
               3-jobs)

  (test-pred "Get a job that has not been started yet"
             job-can-start?
             (get-next-job 2-jobs))


  (test-equal? "Get a job by ID"
               (job-ref 2-jobs 0)
               (job 0 0 'new 'A (seteq)))

  (test-equal? "Rotate assignees for round robin"
               (job-ref 3-jobs 2)
               (job 2 0 'new 'C (seteq)))

  (test-equal? "Update job progress"
               (job-ref (set-job-progress 3-jobs 1 0.5) 1)
               (job 1 1 0.5 'B (seteq)))

  (test-equal? "Update job progress with a job instance"
               (job-ref (set-job-progress 3-jobs (job 1 #f #f #f (seteq)) 'done) 1)
               (job 1 1 'done 'B (seteq)))

  (test-true "Detect when all jobs are done"
             (all-jobs-done? (mark-jobs-done 3-jobs 0 1 2)))


  ; Mark the first job as blocked by the second (zero-based) in a couple of ways.
  (for ([2-jobs/dependent (in-list (list (add-blocker 2-jobs 0 1)
                                         (add-blocking-job 1-job 0 'B)))])
    (test-eq? "Never return a dependent job"
              (get-next-job 2-jobs/dependent)
              (job-ref 2-jobs/dependent 1))

    (test-pred "Move to once-dependent jobs once blockers are clear"
               (λ (pool)
                 (eq? (get-next-job pool)
                      (job-ref pool 0)))
               (set-job-progress 2-jobs/dependent 1 'done))))
