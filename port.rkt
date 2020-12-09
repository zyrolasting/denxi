#lang racket/base

; Extend racket/port to support byte transfers with safety limits.

(require racket/port
         "contract.rkt"
         "message.rkt"
         "string.rkt")

(provide (all-from-out racket/port)
         (contract-out
          [mebibytes->bytes
           (-> real? (or/c +inf.0 exact-nonnegative-integer?))]
          [transfer
           (-> input-port?
               output-port?
               #:on-status (-> $transfer? any)
               #:max-size (or/c +inf.0 exact-positive-integer?)
               #:buffer-size exact-positive-integer?
               #:transfer-name non-empty-string?
               #:est-size (or/c +inf.0 real?)
               #:timeout-ms (>=/c 0)
               void?)]))

(define+provide-message $transfer ())
(define+provide-message $transfer:scope $transfer (name message))
(define+provide-message $transfer:progress $transfer (bytes-read max-size timestamp))
(define+provide-message $transfer:timeout $transfer (bytes-read wait-time))
(define+provide-message $transfer:budget $transfer (allowed-max-size))
(define+provide-message $transfer:budget:exceeded $transfer:budget (overrun-size))
(define+provide-message $transfer:budget:rejected $transfer:budget (proposed-max-size))

(define (mebibytes->bytes mib)
  (if (equal? mib +inf.0)
      mib
      (inexact->exact (ceiling (* mib 1024 1024)))))

(define (transfer from to
                  #:on-status on-status
                  #:max-size max-size
                  #:buffer-size buffer-size
                  #:transfer-name transfer-name
                  #:est-size est-size
                  #:timeout-ms timeout-ms)
  (define (on-status/void m)
    (on-status ($transfer:scope transfer-name m))
    (void))

  (if (<= est-size max-size)
      (copy-port/incremental from to
                             #:on-status on-status/void
                             #:bytes-read 0
                             #:max-size (min est-size max-size)
                             #:buffer-size buffer-size
                             #:buffer (make-bytes buffer-size 0)
                             #:timeout timeout-ms)
      (on-status/void ($transfer:budget:rejected max-size est-size))))


(define (copy-port/incremental
         from to
         #:bytes-read bytes-read
         #:on-status on-status
         #:max-size max-size
         #:buffer-size buffer-size
         #:buffer buffer
         #:timeout timeout)
  (sync (handle-evt
         (alarm-evt (+ (current-inexact-milliseconds)
                       timeout))
         (λ (e)
           (on-status ($transfer:timeout bytes-read timeout))))
        (handle-evt
         (read-bytes-avail!-evt buffer from)
         (λ (variant)
           (cond [(eof-object? variant)
                  (on-status ($transfer:progress max-size max-size (current-seconds)))
                  (void)]
                 [(and (number? variant) (> variant 0))
                  (define bytes-read* (+ bytes-read variant))
                  (write-bytes buffer to 0 variant)
                  ; 100% is always reported at the end. Don't double report it here.
                  (unless (equal? bytes-read* max-size)
                    (on-status ($transfer:progress bytes-read* max-size (current-seconds))))
                  (if (> bytes-read* max-size)
                      (on-status ($transfer:budget:exceeded max-size  (- bytes-read* max-size)))
                      (copy-port/incremental from to
                                             #:bytes-read bytes-read*
                                             #:on-status on-status
                                             #:max-size max-size
                                             #:buffer-size buffer-size
                                             #:buffer buffer
                                             #:timeout timeout))])))))




(module+ test
  (require racket/match
           rackunit)

  (test-case "Convert mebibytes to bytes"
    (check-equal? (mebibytes->bytes +inf.0) +inf.0)
    (check-eq? (mebibytes->bytes 0) 0)
    (check-eqv? (mebibytes->bytes 1)
                1048576)
    (test-equal? "Allow real number expressions for mebibytes"
                 (mebibytes->bytes (/ 1 2))
                 (/ 1048576 2)))

  (test-case "Transfer bytes transparently"
    (define bstr #"ABCDEFG")
    (define bytes/source (open-input-bytes bstr))
    (define bytes/sink (open-output-bytes))
    (define size (bytes-length bstr))
    (define expected-progress-val 0)

    (define (on-status m)
      (check-match m
                   ($transfer:scope "anon"
                                    ($transfer:progress
                                     (? (integer-in 0 size) i)
                                     size
                                     (? exact-integer? _)))))

    (check-pred void?
                (transfer bytes/source bytes/sink
                          #:on-status on-status
                          #:transfer-name "anon"
                          #:buffer-size 1
                          #:timeout-ms 1
                          #:max-size size
                          #:est-size size))

    (check-equal? (get-output-bytes bytes/sink #t) bstr))

  (test-case "Prohibit unlimited transfers unless max-size agrees"
    (transfer (open-input-string "")
              (open-output-nowhere)
              #:transfer-name "anon"
              #:buffer-size 1
              #:timeout-ms 1
              #:max-size 100
              #:est-size +inf.0
              #:on-status (λ (m)
                            (check-equal? m
                             ($transfer:scope "anon"
                                              ($transfer:budget:rejected 100 +inf.0))))))

  (test-case "Time out on reads that block for too long"
    ; Reading from a pipe in this way will block indefinitely.
    (let-values ([(i o) (make-pipe)])
      (transfer i o
                #:transfer-name "anon"
                #:buffer-size 1
                #:timeout-ms 1
                #:max-size 100
                #:est-size 10
                #:on-status
                (λ (m) (check-equal?
                        m
                        ($transfer:scope "anon"
                                         ($transfer:timeout 0 1)))))))

  (test-case "Reject reading too many bytes"
    (define bstr #"ABCDEFG")
    (define bytes/source (open-input-bytes bstr))
    (define bytes/sink (open-output-bytes))
    (define size (bytes-length bstr))
    (check-true
     (call/cc
      (λ (return)
        (transfer bytes/source bytes/sink
                  #:transfer-name "anon"
                  #:buffer-size 5
                  #:timeout-ms 1
                  #:max-size 1
                  #:est-size 1
                  #:on-status
                  (λ (m)
                    (match m
                      [($transfer:scope "anon"
                                        ($transfer:budget:exceeded 1 4))
                       (return #t)]
                      [_ (void)]))))))))
