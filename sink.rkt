#lang racket/base

(require racket/contract
         racket/generic
         "machine.rkt"
         "message.rkt"
         "source.rkt")

(provide gen:sink
         sink?
         sink/c
         sink-drop
         sink-keep
         sink-open
         sink-policy
         sink-source
         (struct-out $open)
         (struct-out memory-sink))

(define-message $open ())

(define-generics sink
  [sink-drop sink]
  [sink-keep sink]
  [sink-open sink]
  [sink-policy sink]
  [sink-source sink])


(struct memory-sink ([data #:mutable] policy)
  #:methods gen:sink
  [(define (sink-source sink)
     (machine
      (λ (state)
        (define data (memory-sink-data sink))
        (if (bytes? data)
            (state-set-value state (byte-source data))
            (state-halt-with state ($open))))))
   (define (sink-open sink)
     (machine
      (λ (state)
        (define out (open-output-bytes))
        (set-memory-sink-data! sink out)
        (state-set-value state out))))
   (define (sink-keep sink)
     (machine
      (λ (state)
        (define data (memory-sink-data sink))
        (unless (bytes? data)
          (set-memory-sink-data! sink (get-output-bytes data #t)))
        state)))
   (define (sink-drop sink)
     (machine-effect (set-memory-sink-data! sink #"")))
   (define (sink-policy sink)
     (machine-rule (memory-sink-policy sink)))])


(module+ test
  (require "test.rkt"
           (submod "machine.rkt" test))

  (test sink-interface
        (define sink (memory-sink #"abc" #t))
        (define source-machine (sink-source sink))
        (define drop-machine (sink-drop sink))
        (define policy-machine (sink-policy sink))
        (define keep-machine (sink-keep sink))
        (define open-machine (sink-open sink))

        (drop-machine)
        (assert (equal? (memory-sink-data sink) #""))
        (assert (state-get-value (policy-machine)))
        (assert (byte-source? (state-get-value (source-machine))))

        (define port (state-get-value (open-machine)))
        (assert (match-halt? (source-machine) ($open)))
        (assert (output-port? port))
        (display "abc" port)
        (close-output-port port)
        (keep-machine)
        (assert (byte-source? (state-get-value (source-machine))))))
