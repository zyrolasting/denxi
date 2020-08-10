#lang racket/base


(provide team%
         suggested-worker-count)

(require racket/class
         racket/place
         racket/runtime-path
         "sentry.rkt"
         "workspace.rkt"
         "zcpkg-messages.rkt")

(define suggested-worker-count
  (max 1 (sub1 (processor-count))))

(define team%
  (class object%
    (super-new)
    (init-field make-place
                [worker-count suggested-worker-count])

    (field [output null]
           [cust (make-custodian)]
           [completion-value 0]
           [workers
            (parameterize ([current-custodian cust])
              (for/list ([id (in-range worker-count)])
                (new sentry%
                     [pch (make-place)]
                     [add-output (λ (v) (set! output (cons v output)))])))])

    (define/public-final (stop!)
      (for ([w (in-list workers)])
        (define subcompletion-value (send w stop!))
        (when (and subcompletion-value
                   (not (eq? subcompletion-value 0)))
          (set! completion-value 1)))
      (custodian-shutdown-all cust)
      (set! workers null)
      completion-value)

    (define/public (broadcast! v)
      (for ([w (in-list workers)])
        (send w value v)))

    (define/public (send-assigned-tasks! tasks)
      (unless (null? workers)
        (set! output null)
        (define num (length workers))

        ; Simple round robin
        (for ([(task index) (in-indexed (in-list tasks))])
          (define id (modulo index num))
          (define sen (list-ref workers id))
          (send sen value task))

        ; Use (min) to ensure $sentinels go to only the workers who got messages.
        (for ([id (in-range (min num (length tasks)))])
          (define sen (list-ref workers id))
          (send sen value ($sentinel)))

        (for ([sen (in-list workers)])
          (send sen wait)))
      output)))


(module+ test
  (require rackunit)

  (define-values (Aouter Ainner) (place-channel))
  (define-values (Bouter Binner) (place-channel))
  (define-values (Couter Cinner) (place-channel))

  (define (consume pch n [accum null])
    (if (= n 0)
        (reverse accum)
        (consume pch
                 (sub1 n)
                 (cons (sync/timeout 0.5 pch)
                       accum))))

  (define (check-evt-val evt v [timeout 0.05])
    (check-equal? v (sync/timeout timeout evt)))

  (define avail (list Aouter Bouter Couter))
  (define (mock-make-place)
    (define v (car avail))
    (set! avail (cdr avail))
    v)

  (define controller
    (new team%
         [make-place mock-make-place]
         [worker-count 3]))

  (send controller broadcast! 'hello)

  (test-true "Broadcast to all workers"
             (andmap (λ (pch) (eq? (sync/timeout 0 pch) 'hello))
                     (list Ainner
                           Binner
                           Cinner)))

  (test-case "Distribute tasks"
    (define th (thread (λ () (send controller send-assigned-tasks! '(a b c a b c a)))))

    (check-equal? (consume Ainner 4) `(a a a ,($sentinel)))
    (check-equal? (consume Binner 3) `(b b ,($sentinel)))
    (check-equal? (consume Cinner 3) `(c c ,($sentinel)))

    ; Places need to echo sentinel values to unblock the sender.
    (check-true (thread-running? th))
    (place-channel-put Ainner ($sentinel))
    (place-channel-put Binner ($sentinel))
    (place-channel-put Cinner ($sentinel))

    (define evt (thread-dead-evt th))
    (check-evt-val evt evt)

    (kill-thread th))

  (test-case "Accumulate output"
    ; Populate program output first, so controller
    ; call does not block indefinitely.
    (place-channel-put Ainner ($output 1))
    (place-channel-put Ainner ($output 2))
    (place-channel-put Ainner ($sentinel))
    (place-channel-put Binner ($output 3))
    (place-channel-put Binner ($sentinel))
    (place-channel-put Cinner ($output 4))
    (place-channel-put Cinner ($output 5))
    (place-channel-put Cinner ($output 6))
    (place-channel-put Cinner ($sentinel))

    (check-equal? (sort (send controller send-assigned-tasks! '(x y z)) <)
                  '(1 2 3 4 5 6))

    (check-equal? (consume Ainner 2) `(x ,($sentinel)))
    (check-equal? (consume Binner 2) `(y ,($sentinel)))
    (check-equal? (consume Cinner 2) `(z ,($sentinel))))

  (test-case "Shut down"
    (send controller stop!)

    ; No $sentinels follow; nothing will be around to listen
    ; for the echo.
    (check-evt-val Ainner ($stop))
    (check-evt-val Binner ($stop))
    (check-evt-val Cinner ($stop))

    ; Broadcasts won't go out.
    (send controller broadcast! 'hello)
    (check-evt-val Ainner #f)
    (check-evt-val Binner #f)
    (check-evt-val Cinner #f)

    ; Same for tasks. Waits won't block, since there is nothing to wait on.
    (define th (thread (λ () (send controller send-assigned-tasks! '(a b c a b c a)))))
    (define evt (thread-dead-evt th))
    (check-evt-val evt evt)
    (kill-thread th)

    ; Output will now repeat.
    (check-eq? (send controller send-assigned-tasks! '(x))
               (send controller send-assigned-tasks! '(y)))))
