#lang racket/base
; Define a model for parallel tasks. Support dependent work.

(provide company-finish-jobs!
         make-company
         company-stop
         get-next-message
         in-schedule
         (struct-out $schedule)
         (struct-out company))

(require (only-in racket/future processor-count)
         racket/generator
         racket/list
         racket/match
         racket/place
         racket/match
         "config.rkt"
         "message.rkt"
         "message-pump.rkt"
         "string.rkt"
         "worker.rkt")

(struct company (message-handler jobs backlog workers)
  #:transparent
  #:property prop:evt
  (λ (self) (apply choice-evt (company-workers self))))


(define (company-finish-jobs! team)
  (let loop ([wip team])
    (if (and (andmap worker-idle? (company-workers wip))
             (null? (company-jobs wip)))
        wip
        (loop (company-iterate wip)))))


(define (make-company #:config [config #f] message-handler messages make-place)
  (company message-handler
           (map (λ (m) ($schedule m null)) messages)
           null
           (for/list ([id (in-range (processor-count))])
             (define w (worker id (make-place) #f))
             (w ($start id config)))))


(define (company-stop team)
  (for ([w (in-list (company-workers team))])
    (define ch (worker-channel w))
    (place-channel-put ch ($stop))
    (or (sync/timeout 0.5 (place-dead-evt ch))
        (place-kill ch)))
  (company (company-message-handler team)
           null
           null
           null))


(define limited
  (let ([n 0])
    (λ (v)
      (when (> n 10)
        (error "done"))
      (writeln v)
      (set! n (add1 n)))))

(define (company-iterate team)
  (let ([variant (sync team)])
    (cond [($message? variant)
           ((company-message-handler team) team variant)]
          [(input-port? variant)
           (displayln (read-line variant))
           team]
          [else (write variant)
                team])))


(define (get-next-message node)
  (define deps ($schedule-dependencies node))
  (define first-dep (if (null? deps) #f (car deps)))
  (if first-dep
      (let-values ([(next-job variant) (get-next-message first-dep)])
        (values next-job
                ($schedule ($schedule-dependent node)
                           (if variant
                               (cons variant (cdr deps))
                               (cdr deps)))))
      (values node #f)))


(define (in-schedule s)
  (in-generator
   (let loop ([remaining s])
     (and remaining
          (let-values ([(next remaining*) (get-next-message remaining)])
            (yield ($schedule-dependent next))
            (loop remaining*))))))


(define (frontlog team schedule)
  (struct-copy company team
               [jobs (cons schedule
                           (company-jobs team))]))


(define (backlog team schedule)
  (struct-copy company team
               [backlog (cons schedule
                              (company-backlog team))]))


(define (done team id finished-job)
  (define jobs (company-jobs team))
  (define reporter (list-ref (company-workers team) id))
  (if (null? jobs)
      (begin (reporter #f)
             team)
      (let*-values ([(next-job remaining) (get-next-message (car jobs))])
        (reporter ($schedule-dependent next-job))
        (struct-copy company team
                     [jobs (if remaining
                               (cons remaining (cdr jobs))
                               (cdr jobs))]))))


(define (crash team id exn-string)
  (company-stop team)
  (raise (exn:fail:user
          (format "A worker crashed. Please report this to the developer:~n~a~n"
                  exn-string)
          (current-continuation-marks))))


(define-message-pump (handle-company-message company? default-message-handler)
  done
  frontlog
  backlog
  crash)


(module+ test
  (require rackunit
           racket/sequence)

  (test-case "Resolve dependencies first"
    (define $ $schedule)

    (define jobs
      ($ 'root
         (list
          ($ 'a
             (list ($ 'x null)
                   ($ 'y null)
                   ($ 'z
                      (list ($ 'q null)))))
          ($ 'b
             (list ($ 'r null)
                   ($ 's (list ($ 'q null))))))))


    (check-equal?
     (sequence->list (in-schedule jobs))
     '(x y q z a r q s b root)))

  (define-message $output (what))
  (define-message $middle (what))

  (define (middle state what)
    (if (eq? what 'fin)
        (state ($backlog  ($schedule ($output what) null)))
        (state ($frontlog ($schedule ($middle 'fin) null)))))

  (define-message-pump (handle worker-state? handle-worker-message)
    middle)

  (define (make-place)
    (place pch (worker-main pch handle)))

  ; Don't run in the top level, because
  ; place creation will not terminate.
  (define (run-place-tests)
    (define team
      (make-company handle-company-message
                    (list ($middle 'a)
                          ($middle 'b))
                    make-place))

    (define installations-done (company-finish-jobs! team))

    (test-true "Schedule initial jobs as if they have no dependencies"
               (andmap (λ (m) (and ($schedule? m)
                                   (null? ($schedule-dependencies m))))
                       (company-jobs team)))

    (test-equal? "Do all jobs"
                 (company-jobs installations-done)
                 null)

    (test-equal? "Populate backlog with output messages"
                 (company-backlog installations-done)
                 (list ($schedule ($output 'fin) null)
                       ($schedule ($output 'fin) null)))))
