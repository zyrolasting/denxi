#lang racket/base

; Define a class to communicate with places, with added semaphore-like
; behavior: The class should consider the place "idle" if it receives
; as many of a certain message as it sends. This allows a place to
; signal that it is not only done with a task, but that it also has no
; further messages to send.

(provide (all-defined-out))
(require racket/place
         "actor.rkt"
         "message.rkt")

(define-message $sentinel ())
(define-message $start    (workspace-directory))
(define-message $fail     (to-display))
(define-message $output   (v))
(define-message $stop     ())

; Communicate only on place channel. Avoid ports for simplicity.
(define sentry%
  (class actor%
    (super-new)
    (init-field pch add-output)

    (inherit pump)

    (field [counter 0]
           [output null])

    (define/public (stop!)
      (when pch
        (to-place! ($stop))
        (when (place? pch) ; Place channels are not necessarily places.
          (or (sync/timeout 0.5 (place-dead-evt pch))
              (place-kill pch)))
        (set! pch #f))
      this)

    (define/public (wait)
      (unless (balanced?)
        (pump (sync pch))
        (wait)))

    ; For external users, (send worker value v) is easy to understand.
    ; (send worker to-place! v) is confusing.  (to-place! v) is better
    ; for inside of this class.
    (define/private (to-place! v)
      (when ($sentinel? v)
        (set! counter (add1 counter)))
      (place-channel-put pch v)
      this)

    (define/public (value v)
      (to-place! v))

    (define/public (get-evt)
      pch)

    (define/public (handle-$sentinel)
      (set! counter (sub1 counter)))

    (define/public (handle-$output v)
      (add-output v))

    (define/public (get-balance)
      counter)

    (define/public (balanced?)
      (= (get-balance) 0))))


(module+ test
  (require rackunit
           racket/file)

  (test-case "Carry sentry% through place lifecycle"
    (define-values (outside-place inside-place) (place-channel))
    (define output null)

    (define op
      (new sentry%
           [pch outside-place]
           [add-output (λ (v) (set! output (cons v output)))]))

    (send op value 'hello)
    (test-eq? "Communicate over channel through sentry%"
              (sync/timeout 0 inside-place)
              'hello)

    (test-eq?
     "Start a sentry balanced"
     (send op get-balance)
     0)

    (send op value ($sentinel))
    (send op value ($sentinel))

    (test-eq? "Add to sentry balance"
              (send op get-balance)
              2)

    ; Simulate place responding with $sentinel
    (place-channel-put inside-place ($sentinel))
    (place-channel-put inside-place ($output 1))
    (place-channel-put inside-place ($output 2))
    (place-channel-put inside-place ($output 3))
    (send op pump (sync (send op get-evt)))
    (send op pump (sync (send op get-evt)))
    (send op pump (sync (send op get-evt)))
    (send op pump (sync (send op get-evt)))

    (test-equal? "Collect output" output '(3 2 1))
    (test-eq? "Do not consider work balanced yet (One more $sentinel needed)"
              (send op get-balance) 1)

    (place-channel-put inside-place ($sentinel))
    (send op pump (sync (send op get-evt)))

    (test-eq? "Restore sentry balance" (send op get-balance) 0)
    (test-true "Make balanced? an alias for (= balance 0)"
               (send op balanced?))

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
          (new sentry%
               [pch (begin (set! stuck (dynamic-place tmp-file 'm))
                           stuck)]
               [add-output void]))

        (define dead-evt (place-dead-evt stuck))

        (test-false "Make place that does not terminate"
                    (sync/timeout 0.1 dead-evt))

        (send killer stop!)

        (test-eq? "sentry% will kill a zombie place"
                  dead-evt
                  (sync/timeout 0 dead-evt)))
      (λ ()
        (when (place? stuck)
          (place-kill stuck)) ; for good measure
        (when (file-exists? tmp-file)
          (delete-file tmp-file))))

    (void)))
