#lang racket/base

; Extend racket/port to support configurable transfers.

(require racket/port
         "contract.rkt"
         "string.rkt"
         "exn.rkt")

(provide (all-from-out racket/port)
         (contract-out
          [transfer
           (-> input-port?
               output-port?
               #:max-size (or/c +inf.0 exact-positive-integer?)
               #:buffer-size exact-positive-integer?
               #:transfer-name non-empty-string?
               #:est-size (or/c +inf.0 real?)
               #:timeout-ms (>=/c 0)
               #:on-progress (-> (real-in 0 1) any)
               exact-positive-integer?)]))

(define-exn exn:fail:user:xiden:transfer exn:fail:user:xiden (name))
(define-exn exn:fail:user:xiden:transfer:timeout exn:fail:user:xiden:transfer ())
(define-exn exn:fail:user:xiden:transfer:too-big exn:fail:user:xiden:transfer ())
(define-exn exn:fail:user:xiden:transfer:unlimited-size exn:fail:user:xiden:transfer ())

(define (transfer from to
                  #:max-size max-size
                  #:buffer-size buffer-size
                  #:transfer-name transfer-name
                  #:est-size est-size
                  #:timeout-ms timeout-ms
                  #:on-progress on-progress)
  (define (fail ctor)
    (rex ctor transfer-name))

  (define normalized-max-size
    (if (eq? est-size +inf.0)
        (if (eq? est-size max-size)
            max-size
            (fail exn:fail:user:xiden:transfer:unlimited-size))
        (min est-size max-size)))

  (copy-port/incremental from to
                         #:on-progress on-progress
                         #:transfer-name transfer-name
                         #:fail fail
                         #:bytes-read 0
                         #:max-size normalized-max-size
                         #:buffer-size buffer-size
                         #:buffer (make-bytes buffer-size 0)
                         #:timeout timeout-ms))



(define (copy-port/incremental
         from to
         #:on-progress on-progress
         #:transfer-name transfer-name
         #:fail fail
         #:bytes-read bytes-read
         #:max-size max-size
         #:buffer-size buffer-size
         #:buffer buffer
         #:timeout timeout)
  (on-progress transfer-name (/ bytes-read max-size))
  (if (> bytes-read max-size)
      (fail exn:fail:user:xiden:transfer:too-big)
      (sync (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                                      timeout))
                        (λ (e) (fail exn:fail:user:xiden:transfer:timeout)))
            (handle-evt (read-bytes-avail!-evt buffer from)
                        (λ (variant)
                          (cond [(eof-object? variant)
                                 bytes-read]
                                [(and (number? variant) (> variant 0))
                                 (write-bytes buffer to 0 variant)
                                 (copy-port/incremental from to
                                                        #:on-progress on-progress
                                                        #:transfer-name transfer-name
                                                        #:fail fail
                                                        #:bytes-read (+ bytes-read variant)
                                                        #:max-size max-size
                                                        #:buffer-size buffer-size
                                                        #:buffer buffer
                                                        #:timeout timeout)]))))))

(module+ test
  (require rackunit)

  (test-case "Transfer bytes transparently"
    (define bstr #"ABCDEFG")
    (define bytes/source (open-input-bytes bstr))
    (define bytes/sink (open-output-bytes))
    (define size (bytes-length bstr))
    (define expected-progress-val 0)

    (define (on-progress name v)
      (check-equal? name "anon")
      (check-equal? v expected-progress-val)
      (set! expected-progress-val
            (+ expected-progress-val
               (/ 1 size))))

    (define written
      (transfer bytes/source bytes/sink
                #:transfer-name "anon"
                #:buffer-size 1
                #:timeout-ms 1
                #:max-size size
                #:est-size size
                #:on-progress on-progress))

    (check-eq? written size)
    (check-equal? (get-output-bytes bytes/sink #t) bstr))

  (define (expect-exn predicate)
    (λ (e)
      (and (predicate e)
           (exn:fail:user:xiden:transfer? e)
           (equal? (exn:fail:user:xiden:transfer-name e)
                   "anon"))))

  (test-exn "Prohibit unlimited transfers unless max-size agrees"
            (expect-exn exn:fail:user:xiden:transfer:unlimited-size?)
            (λ ()
              (transfer (open-input-string "")
                        (open-output-nowhere)
                        #:transfer-name "anon"
                        #:buffer-size 1
                        #:timeout-ms 1
                        #:max-size 100
                        #:est-size +inf.0
                        #:on-progress void)))

    (test-exn "Time out on reads that block for too long"
            (expect-exn exn:fail:user:xiden:transfer:timeout?)
            (λ ()
              ; Reading from a pipe in this way will block indefinitely.
              (define-values (i o) (make-pipe))
              (transfer i o
                        #:transfer-name "anon"
                        #:buffer-size 1
                        #:timeout-ms 1
                        #:max-size 100
                        #:est-size 10
                        #:on-progress void)))

    (test-exn "Reject reading too many bytes"
              (expect-exn exn:fail:user:xiden:transfer:too-big?)
              (λ ()
                (define bstr #"ABCDEFG")
                (define bytes/source (open-input-bytes bstr))
                (define bytes/sink (open-output-bytes))
                (define size (bytes-length bstr))
                (transfer bytes/source bytes/sink
                          #:transfer-name "anon"
                          #:buffer-size 1
                          #:timeout-ms 1
                          #:max-size 1
                          #:est-size size
                          #:on-progress void))))
