#lang racket/base

(provide call-with-time-limit
         call-with-memory-limit
         call-with-environment-variable-subset)

(require racket/function)

(define (call-with-time-limit time-limit timed-out continue)
  (define return-values
    values)

  (define timed-thread
    (thread (thunk (call-with-values continue
                                     (λ A (set! return-values (λ () (apply values A))))))))

  (sync/enable-break (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                                               (* time-limit 1000)))
                                 (λ _
                                   (kill-thread timed-thread)
                                   (timed-out)))
                     (handle-evt (thread-dead-evt timed-thread)
                                 (λ _
                                   (return-values)))))


(define (call-with-memory-limit limit continue)
  (define stop-cust #f)
  (dynamic-wind (λ ()
                  (set! stop-cust (make-custodian))
                  (custodian-limit-memory stop-cust limit stop-cust))
                (λ ()
                  (parameterize ([current-custodian stop-cust])
                    (continue)))
                (λ ()
                  (when (custodian? stop-cust)
                    (custodian-shutdown-all stop-cust)))))


(define (call-with-environment-variable-subset input-set allowed continue)
  (parameterize ([current-environment-variables
                  (apply make-environment-variables
                         (for/fold ([mappings null])
                                   ([name (in-list allowed)])
                           (define value (environment-variables-ref input-set name))
                           (if value
                               (cons name
                                     (cons (environment-variables-ref input-set name)
                                           mappings))
                               mappings)))])
    (continue)))
