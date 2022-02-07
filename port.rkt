#lang racket/base

(require racket/contract
         racket/port
         "message.rkt")

(provide (all-from-out racket/port)
         (struct-out transfer-policy)
         (struct-out $transfer)
         (struct-out $transfer:scope)
         (struct-out $transfer:broken)
         (struct-out $transfer:progress)
         (struct-out $transfer:timeout)
         (struct-out $transfer:budget)
         (struct-out $transfer:budget:exceeded)
         (struct-out $transfer:budget:rejected)
         (contract-out
          [full-trust-transfer-policy
           transfer-policy/c]
          [mebibytes->bytes
           (-> real? (or/c +inf.0 exact-nonnegative-integer?))]
          [transfer
           (-> input-port?
               output-port?
               (or/c +inf.0 exact-nonnegative-integer?)
               transfer-policy?
               void?)]
          [transfer-policy/c
           contract?]
          [zero-trust-transfer-policy
           transfer-policy/c]))

(define-message $transfer ())
(define-message $transfer:scope $transfer (name timestamp-s message))
(define-message $transfer:broken $transfer (value))
(define-message $transfer:progress $transfer (bytes-read max-size))
(define-message $transfer:timeout $transfer (bytes-read wait-time))
(define-message $transfer:budget $transfer (allowed-max-size))
(define-message $transfer:budget:exceeded $transfer:budget (overrun-size))
(define-message $transfer:budget:rejected $transfer:budget (proposed-max-size))

(require racket/match
         "string.rkt")

(struct transfer-policy
   (buffer-size
    max-size
    name
    timeout-ms
    telemeter))

(define zero-trust-transfer-policy
  (transfer-policy 1 0 "" 0 void))

(define full-trust-transfer-policy
  (transfer-policy 8192 +inf.0 "" +inf.0 void))

(define transfer-policy/c
  (struct/c transfer-policy
            exact-positive-integer?
            (or/c +inf.0 exact-nonnegative-integer?)
            string?
            (>=/c 0)
            (-> $transfer? void?)))



(define ((progress-printer formatter) m)
  (if ($transfer:progress? ($transfer:scope-message m))
      (printf "\r~a~a" (formatter m)
              (if (equal? ($transfer:progress-bytes-read ($transfer:scope-message m))
                          ($transfer:progress-max-size ($transfer:scope-message m)))
                  "\n" ""))
      (displayln (formatter m))))


(define (transfer from to est-size policy)
  (match-let* ([(transfer-policy buffer-size max-size name timeout-ms telemeter)
               policy]
               [telemeter*
                (λ (message)
                  (telemeter ($transfer:scope name (current-seconds) message)))])
    (if (<= est-size max-size)
        (copy-port/incremental from
                               to
                               0
                               (make-bytes buffer-size 0)
                               buffer-size
                               (min est-size max-size)
                               name
                               timeout-ms
                               telemeter*)
        (telemeter* ($transfer:budget:rejected max-size est-size)))))


(define (copy-port/incremental
         from
         to
         bytes-read
         buffer
         buffer-size
         max-size
         name
         timeout-ms
         telemeter)
  (sync (handle-evt
         (alarm-evt (+ (current-inexact-milliseconds)
                       timeout-ms))
         (λ (e)
           (telemeter ($transfer:timeout bytes-read timeout-ms))))
        (handle-evt
         (read-bytes-avail!-evt buffer from)
         (λ (variant)
           (cond [(eof-object? variant)
                  (telemeter ($transfer:progress max-size max-size))]
                 [((>/c 0) variant)
                  (define bytes-read* (+ bytes-read variant))
                  (write-bytes buffer to 0 variant)
                  ; 100% is always reported at the end. Don't double report it here.
                  (unless (equal? bytes-read* max-size)
                    (telemeter ($transfer:progress bytes-read* max-size)))
                  (if (> bytes-read* max-size)
                      (telemeter ($transfer:budget:exceeded max-size (- bytes-read* max-size)))
                      (copy-port/incremental
                       from
                       to
                       bytes-read*
                       buffer
                       buffer-size
                       max-size
                       name
                       timeout-ms
                       telemeter))]
                 [else
                  (telemeter ($transfer:broken variant))])))))


(define (mebibytes->bytes mib)
  (if (equal? mib +inf.0)
      mib
      (inexact->exact (ceiling (* mib 1024 1024)))))


(module+ test
  (require racket/function
           "test.rkt")
    
  (test mebibytes->bytes
    (assert (equal? (mebibytes->bytes +inf.0) +inf.0))
    (assert (eq? (mebibytes->bytes 0) 0))
    (assert (eqv? (mebibytes->bytes 1) 1048576))
    (assert (equal? (mebibytes->bytes (/ 1 2)) (/ 1048576 2))))

  (define-syntax-rule (overturn instance . assignments)
    (struct-copy transfer-policy instance . assignments))

  (define-syntax-rule (draft-transfer-policy . xs)
    (overturn zero-trust-transfer-policy . xs))
  
  (define test-policy
    (draft-transfer-policy
     [name "anon"]
     [buffer-size 1]
     [timeout-ms 1]))

  (define-syntax-rule (P . x)
    (overturn test-policy . x))

  (define ((expect-message m) telemeasure)
    (check-equal? telemeasure
                  ($transfer:scope (transfer-policy-name test-policy)
                                   ($transfer:scope-timestamp-s telemeasure)
                                   m)))
  
  (test-case "Transfer bytes with telemetry"
    (define bstr #"ABCDEFG")
    (define bytes/source (open-input-bytes bstr))
    (define bytes/sink (open-output-bytes))
    (define size (bytes-length bstr))

    (define (telemeter m)
      (check-match m
                   ($transfer:scope (? (curry equal? (transfer-policy-name test-policy)) _)
                                    (? exact-positive-integer? _)
                                    ($transfer:progress
                                     (? (integer-in 0 size) i)
                                     size))))

    (check-pred void?
                (transfer bytes/source
                          bytes/sink
                          size
                          (P [buffer-size size]
                             [max-size size]
                             [telemeter telemeter])))

    (check-equal? (get-output-bytes bytes/sink #t) bstr))


  (test-case "Prohibit unlimited transfers unless max-size agrees"
    (transfer (open-input-string "")
              (open-output-nowhere)
              +inf.0
              (P [max-size 100]
                 [telemeter
                  (expect-message ($transfer:budget:rejected 100 +inf.0))])))

  (test-case "Time out on reads that block for too long"
    ; Reading from a pipe in this way will block indefinitely.
    (let-values ([(i o) (make-pipe)])
      (transfer i o 2
                (overturn test-policy
                          [max-size 2]
                          [telemeter
                           (expect-message ($transfer:timeout 0 1))]))))
  
  (test-case "Reject jobs that exceed the budget"
    (define bstr #"ABCDEFG")
    (define bytes/source (open-input-bytes bstr))
    (define bytes/sink (open-output-bytes))
    (define size (bytes-length bstr))
    (check-true
     (let/cc return
        (transfer bytes/source bytes/sink 1
                  (P [buffer-size 5]
                     [max-size 7]
                     [telemeter
                      (λ (message)
                        (match ($transfer:scope-message message)
                          [($transfer:budget:exceeded 1 4)
                           (return #t)]
                          [_ (void)]))]))))))
