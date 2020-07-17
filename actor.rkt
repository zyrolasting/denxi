#lang racket/base

; Define objects for use in parallel programming.  Focus on an
; interface that keeps the lifecycle and message passing model easy to
; follow.
;
; Note: I did not have much success using structs and functional
; updates, because functional code mixed with imperative code to an
; exhausting degree. I use OOP and imperative code here because it is
; easier to follow.

(provide actor%
         outside-place%
         team%
         (all-from-out racket/class))

(require racket/class
         racket/place
         (only-in racket/future processor-count)
         "message.rkt"
         "string.rkt")


(define actor%
  (class object%
    (abstract idle? recv)
    (define/public-final (loop)
      (if (idle?) this
          (let-values ([(method args) (destructure-message (sync (recv)))])
            (apply dynamic-send this method args)
            (loop))))
    (super-new)))


; Communicate only on place channel. Avoid ports for simplicity.
(define outside-place%
  (class actor%
    (super-new)
    (init-field make-place
                make-start-message
                make-stop-message)
    (field [pch #f])

    (define/public (start!)
      (stop!)
      (set! pch (make-place))
      (to-place! (make-start-message))
      this)

    (define/public (stop!)
      (when pch
        (to-place! (make-stop-message))
        (when (place? pch) ; Place channels are not necessarily places.
          (or (sync/timeout 0.5 (place-dead-evt pch))
              (place-kill pch)))
        (set! pch #f))
      this)

    ; For external users, (send worker value v) is easy to understand.
    ; (send worker to-place! v) is confusing.  (to-place! v) is better
    ; for inside of this class.
    (define/private (to-place! v)
      (place-channel-put pch v)
      this)

    (define/public (value v)
      (to-place! v))

    (define/override (recv)
      pch)

    (define/override (idle?)
      (not pch))))

; Define a way to manage several actor% instances.

(define suggested-worker-count
  (max 1 (sub1 (processor-count))))

(define team%
  (class actor%
    (super-new)
    (field [workers null])

    (define/public-final (start! make-worker [worker-count suggested-worker-count])
      (stop!)
      (set! workers
            (for/list ([id (in-range worker-count)])
              (define w (make-worker))
              (send w start!)
              w)))

    (define/public-final (stop!)
      (for ([w (in-list workers)])
        (send w stop!))
      (set! workers null))

    (define/override (recv)
      (apply choice-evt
             (map (λ (w) (send w recv))
                  workers)))

    (define/override (idle?)
      (andmap (λ (w) (send w idle?)) workers))))


(module+ test
  (require racket/file
           racket/set
           rackunit
           rackunit/text-ui)

  (define-message $start (v))
  (define-message $doing (v))
  (define-message $done  (v))

  (test-case "Call actor% methods with messages"
    (define dumb-actor%
      (class actor%
        (init-field messages)
        (define/override (idle?)
          (null? messages))
        (define/override (recv)
          (handle-evt
           always-evt
           (λ _
             (and (not (idle?))
                  (let ([v (car messages)])
                    (set! messages (cdr messages))
                    v)))))
        (define/public ($start a)
          (test-eq? "Got $start value" a 1))
        (define/public ($doing a)
          (test-eq? "Got $doing value" a 2))
        (define/public ($done a)
          (test-eq? "Got $done value" a 3))
        (super-new)))

    (send (new dumb-actor%
               [messages (list ($start 1)
                               ($doing 2)
                               ($done 3))])
          loop)
    (void))

  (test-case "Carry outside-place% through place lifecycle"
    (define-values (outside-place inside-place) (place-channel))


    (define op
      (new (class outside-place%
             (super-new)
             (inherit stop!)
             (define/public ($start a)
               (test-eq? "Got $start value" a 1))
             (define/public ($doing a)
               (test-eq? "Got $doing value" a 2))
             (define/public ($done a)
               (test-eq? "Got $done value" a 3)
               (stop!)))
           [make-place (λ () outside-place)]
           [make-start-message (λ () ($start 1))]
           [make-stop-message (λ () ($done 2))]))

    (send op start!)

    (define maybe-$start (sync/timeout 0 inside-place))

    (test-pred "Send start message to place on start!"
               $start?
               maybe-$start)

    (place-channel-put inside-place maybe-$start)
    (place-channel-put inside-place ($doing 2))
    (place-channel-put inside-place ($done 3))

    (send op value 'hello)
    (test-eq? "Communicate over channel through outside-place%"
              (sync/timeout 0 inside-place)
              'hello)

    (send op loop)

    (define tmp-file (make-temporary-file))

    ; This place will never terminate
    (display-to-file
     #:exists 'truncate/replace
     "#lang racket/base\n(provide m) (define (m _) (sync never-evt))"
     tmp-file)

    (define stuck #f)

    (dynamic-wind
      void
      (λ ()
        (define killer
          (new outside-place%
               [make-place (λ ()
                             (set! stuck (dynamic-place tmp-file 'm))
                             stuck)]
               [make-start-message (λ () 'whatever)]
               [make-stop-message (λ () 'whatever)]))


        (send killer start!)

        (define dead-evt (place-dead-evt stuck))

        (test-false "Make place that does not terminate"
                    (sync/timeout 0.1 dead-evt))

        (send killer stop!)

        (test-eq? "outside-place% will kill a zombie place"
                  dead-evt
                  (sync/timeout 0 dead-evt)))
      (λ ()
        (when (place? stuck)
          (place-kill stuck)) ; for good measure
        (when (file-exists? tmp-file)
          (delete-file tmp-file))))

    (void))


  (test-case "Manage team lifecycle"
    (define counter 0)
    (define recvd (mutable-set))
    (define recv-count 0)

    (define mock-worker%
      (class actor%
        (super-new)
        (field [started #f]
               [stopped #f]
               [id (begin0 counter
                     (set! counter (add1 counter)))])
        (define/public (start!)
          (set! started #t))
        (define/public (stop!)
          (set! stopped #t))
        (define/override (idle?)
          (> recv-count 10))
        (define/override (recv)
          (handle-evt always-evt
                      (λ _
                        (set! recv-count (add1 recv-count))
                        (set-add! recvd id)
                        id)))))

    (define team (new team%))

    (send team start! (λ () (new mock-worker%)))
    (define workers (get-field workers team))
    (test-true "Start all workers on instantiation"
               (andmap (λ (w) (get-field started w))
                       workers))

    (test-= "Created the requested number of workers"
            counter
            suggested-worker-count
            0)

    (for ([i (in-range 100)])
      (sync (send team recv)))


    (test-true "Pulled from more than one worker"
               (> (set-count recvd) 1))

    (test-true "Workers may be considered idle, even when polled after the fact"
               (andmap (λ (w) (send w idle?))
                       workers))

    (test-true "Flag a team idle if its workers are idle"
               (send team idle?))

    (send team stop!)
    (test-true "Stop all workers when team stops"
               (andmap (λ (w) (get-field started w))
                       workers))))
