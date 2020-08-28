#lang racket/base

; Extend racket/port to support functional transfers with safety limits.

(require racket/port
         "contract.rkt"
         "message.rkt"
         "string.rkt")


(provide (all-from-out racket/port)
         (contract-out
          [mibibytes->bytes
           (-> real? exact-nonnegative-integer?)]
          [transfer
           (-> input-port?
               output-port?
               #:max-size (or/c +inf.0 exact-positive-integer?)
               #:buffer-size exact-positive-integer?
               #:transfer-name non-empty-string?
               #:est-size (or/c +inf.0 real?)
               #:timeout-ms (>=/c 0)
               ($with-messages/c exact-positive-integer?))]))


(define+provide-message $transfer (name))
(define+provide-message $transfer-progress $transfer (scalar timestamp))
(define+provide-message $transfer-small-budget $transfer ())
(define+provide-message $transfer-over-budget $transfer ())
(define+provide-message $transfer-timeout $transfer ())


(define (mibibytes->bytes mib)
  (inexact->exact (ceiling (* mib 1024 1024))))

(define (transfer from to
                  #:max-size max-size
                  #:buffer-size buffer-size
                  #:transfer-name transfer-name
                  #:est-size est-size
                  #:timeout-ms timeout-ms)
  (define budget-or-#f (make-transfer-budget est-size max-size))
  (if (not budget-or-#f)
      (attach-message 0 ($transfer-small-budget transfer-name))
      (imperative-style ; Because this a pain to do otherwise
       (copy-port/incremental from to
                              #:transfer-name transfer-name
                              #:bytes-read 0
                              #:max-size budget-or-#f
                              #:buffer-size buffer-size
                              #:buffer (make-bytes buffer-size 0)
                              #:timeout timeout-ms))))


; #f if the estimated size and max size don't agree.
(define (make-transfer-budget est-size max-size)
  (if (eq? est-size +inf.0)
      (and (eq? est-size max-size)
           max-size)
      (and (<= est-size max-size)
           (min est-size max-size))))


(define (copy-port/incremental
         from to
         #:transfer-name transfer-name
         #:bytes-read bytes-read
         #:max-size max-size
         #:buffer-size buffer-size
         #:buffer buffer
         #:timeout timeout)
  (sync (handle-evt
         (alarm-evt (+ (current-inexact-milliseconds)
                       timeout))
         (λ (e) (emit-message! (attach-message bytes-read ($transfer-timeout transfer-name)))))
        (handle-evt
         (read-bytes-avail!-evt buffer from)
         (λ (variant)
           (cond [(eof-object? variant) bytes-read]
                 [(and (number? variant) (> variant 0))
                  (define bytes-read* (+ bytes-read variant))
                  (if (> bytes-read* max-size)
                      (emit-message! (attach-message bytes-read ($transfer-over-budget transfer-name)))
                      (begin (write-bytes buffer to 0 variant)
                             (emit-message! (attach-message
                                             bytes-read*
                                             ($transfer-progress transfer-name
                                                                 (/ bytes-read* max-size)
                                                                 (current-seconds))))
                             (copy-port/incremental from to
                                                    #:transfer-name transfer-name
                                                    #:bytes-read bytes-read*
                                                    #:max-size max-size
                                                    #:buffer-size buffer-size
                                                    #:buffer buffer
                                                    #:timeout timeout)))])))))



(module+ test
  (require rackunit)

  (test-case "Convert mibibytes to bytes"
    (check-eq? (mibibytes->bytes 0) 0)
    (check-eqv? (mibibytes->bytes 1)
                1048576)
    (test-equal? "Allow real number expressions for mibibytes"
                 (mibibytes->bytes (/ 1 2))
                 (/ 1048576 2)))

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

    (define out
      (transfer bytes/source bytes/sink
                #:transfer-name "anon"
                #:buffer-size 1
                #:timeout-ms 1
                #:max-size size
                #:est-size size))

    (define written ($with-messages-intermediate out))

    (check-eq? written size)
    (check-equal? (get-output-bytes bytes/sink #t) bstr))


  (test-pred "Prohibit unlimited transfers unless max-size agrees"
            $transfer-small-budget?
            (car ($with-messages-accumulated
                  (transfer (open-input-string "")
                            (open-output-nowhere)
                            #:transfer-name "anon"
                            #:buffer-size 1
                            #:timeout-ms 1
                            #:max-size 100
                            #:est-size +inf.0))))

  (test-pred "Time out on reads that block for too long"
             $transfer-timeout?
             ; Reading from a pipe in this way will block indefinitely.
             (car ($with-messages-accumulated
                   (let-values ([(i o) (make-pipe)])
                     (transfer i o
                               #:transfer-name "anon"
                               #:buffer-size 1
                               #:timeout-ms 1
                               #:max-size 100
                               #:est-size 10)))))

  (test-case "Do not encounter off-by-one when reading an exact number of bytes"
    (define bstr #"ABC")
    (define bytes/source (open-input-bytes bstr))
    (define bytes/sink (open-output-bytes))
    (define size (bytes-length bstr))
    (check-true (andmap $transfer-progress?
                        ($with-messages-accumulated
                         (transfer bytes/source bytes/sink
                                   #:transfer-name "anon"
                                   #:buffer-size size
                                   #:timeout-ms 1
                                   #:max-size size
                                   #:est-size size)))))

  (test-case "Reject reading too many bytes"
    (define bstr #"ABCDEFG")
    (define bytes/source (open-input-bytes bstr))
    (define bytes/sink (open-output-bytes))
    (define size (bytes-length bstr))
    (check-pred $transfer-over-budget?
                (car ($with-messages-accumulated
                      (transfer bytes/source bytes/sink
                                #:transfer-name "anon"
                                #:buffer-size 5
                                #:timeout-ms 1
                                #:max-size 1
                                #:est-size 1))))))
