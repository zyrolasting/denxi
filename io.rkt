#lang racket/base

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
         sink-open
         sink-close
         sink-policy
         sink-source
         source?
         source/c
         source-measure
         source-tap
         (contract-out
          [byte-source (-> bytes? prescribed-source?)]
          [conduit? predicate/c]
          [empty-source source?]
          [io (-> source? sink? machine?)]
          [text-source (-> string? source?)]))



(define-generics sink
  [sink-open sink]
  [sink-policy sink]
  [sink-close sink]
  [sink-source sink])


(define-generics source
  [source-measure source]
  [source-tap source])


(define conduit?
  (conjoin sink? source?))


(define (io source sink)
  (mdo proceed? := (machine-rule (state-halt? ((sink-source sink))))
       (if proceed?
           (mdo policy   := (sink-policy sink)
                to-sink  := (sink-open sink)
                est-size := (source-measure source)
                from-tap := (source-tap source)
                (machine-rule (transfer from-tap to-sink est-size policy))
                (sink-close sink))
           (machine-unit #f))))


(define (byte-source data)
  (prescribed-source (bytes-length data)
                     (open-input-bytes data)))


(struct prescribed-source (estimated-size input-port)
  #:methods gen:source
  [(define (source-measure source)
     (machine-rule (prescribed-source-estimated-size source)))
   (define (source-tap source)
     (machine-rule (prescribed-source-input-port source)))])


(struct memory-conduit ([data #:mutable] policy)
  #:methods gen:source
  [(define (source-measure source)
     (machine-rule (bytes-length (memory-conduit-data source))))
   (define (source-tap source)
     (machine-rule (open-input-bytes (memory-conduit-data source))))]

  #:methods gen:sink
  [(define (sink-source sink)
     (machine
      (λ (state)
        (define data (memory-conduit-data sink))
        (state-set-value state
                         (byte-source
                          (if (bytes? data)
                              data
                              (get-output-bytes data #f)))))))

   (define (sink-open sink)
     (machine
      (λ (state)
        (define out (open-output-bytes))
        (set-memory-conduit-data! sink out)
        (state-set-value state out))))

   (define (sink-close sink)
     (machine
      (λ (state)
        (define data (memory-conduit-data sink))
        (unless (bytes? data)
          (set-memory-conduit-data! sink (get-output-bytes data #t)))
        state)))

   (define (sink-policy sink)
     (machine-rule (memory-conduit-policy sink)))])


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
        (define sink (memory-conduit #"" #t))
        (define source-machine (sink-source sink))
        (define policy-machine (sink-policy sink))
        (define open-machine (sink-open sink))

        (assert (equal? (memory-conduit-data sink) #""))
        (assert (state-get-value (policy-machine)))
        (assert (source? (state-get-value (source-machine))))

        (define port (state-get-value (open-machine)))
        (assert (output-port? port))
        (display "abc" port)
        (close-output-port port)
        (assert (source? (state-get-value (source-machine))))))

