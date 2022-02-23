#lang racket/base

; Define I/O in terms of generic sources and sinks.
; Assume sources produce untrusted data.

(require racket/contract
         racket/function
         racket/generic
         racket/match
         "machine.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt")


(provide (struct-out memory-conduit)
         (struct-out prescribed-source)
         gen:sink
         gen:source
         sink?
         sink/c
         sink-drain
         source?
         source/c
         source-measure
         source-tap
         (contract-out
          [byte-source (-> bytes? prescribed-source?)]
          [empty-source source?]
          [text-source (-> string? source?)]))


(define-generics sink
  [sink-drain sink source])


(define-generics source
  [source-measure source]
  [source-tap source])


(define (byte-source data)
  (prescribed-source (bytes-length data)
                     (open-input-bytes data)))


(struct void-source ()
  #:methods gen:source
  [(define (source-measure source)
     (machine-unit halt))
   (define (source-tap source)
     (machine-unit halt))])


(struct prescribed-source (estimated-size input-port)
  #:methods gen:source
  [(define (source-measure source)
     (machine-unit (prescribed-source-estimated-size source)))
   (define (source-tap source)
     (machine-unit (prescribed-source-input-port source)))])


(struct memory-conduit ([data #:mutable] policy)
  #:methods gen:source
  [(define (source-measure source)
     (machine-unit (bytes-length (memory-conduit-data source))))
   (define (source-tap source)
     (machine-unit (open-input-bytes (memory-conduit-data source))))]
  #:methods gen:sink
  [(define (sink-drain sink source)
     (machine
      (λ (state)
        (define to-bytes
          (open-output-bytes))
        (define policy
          (memory-conduit-policy sink))
        (define drain
          (mdo est-size    := (source-measure source)
               from-source := (source-tap source)
               (machine-effect (transfer from-source to-bytes est-size policy))))
        (define state*
          (drain state))
        (unless (state-halt? state*)
          (set-memory-conduit-data! sink (get-output-bytes to-bytes #t)))
        state*)))])


(define empty-source
  (byte-source #""))


(define (text-source data)
  (byte-source (string->bytes/utf-8 data)))


(module+ test
  (require racket/port
           "test.rkt"
           (submod "machine.rkt" test))

  (test source-interface
        (define src (byte-source #"abc"))
        (define tap-machine (source-tap src))
        (define measure-machine (source-measure src))
        (assert (equal? #"abc" (port->bytes (state-get-value (tap-machine)))))
        (assert (= 3 (state-get-value (measure-machine)))))

  (test sink-interface
        (define sink (memory-conduit #"" full-trust-transfer-policy))
        (define drain-machine (sink-drain sink (byte-source #"abc")))
        (assert (equal? (memory-conduit-data sink) #""))
        (drain-machine)
        (assert (equal? (memory-conduit-data sink) #"abc"))))
