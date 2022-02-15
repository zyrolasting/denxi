#lang racket/base

(require racket/contract
         racket/generic
         "machine.rkt")

(provide gen:source
         source/c
         (struct-out byte-source)
         (contract-out
          [empty-source source?]
          [source? predicate/c]
          [text-source (-> string? source?)]))


(define-generics source
  [source-measure source]
  [source-tap source])


(struct byte-source (data)
  #:methods gen:source
  [(define (source-tap source)
     (machine-rule (open-input-bytes (byte-source-data source))))
   (define (source-measure source)
     (machine-rule (bytes-length (byte-source-data source))))])


(define empty-source
  (byte-source #""))


(define (text-source data)
  (byte-source (string->bytes/utf-8 data)))


(module+ test
  (require racket/port
           "test.rkt")

  (test source-interface
        (define src (byte-source #"abc"))
        (define tap-machine (source-tap src))
        (define measure-machine (source-measure src))
        (assert (equal? #"abc" (port->bytes (state-get-value (tap-machine)))))
        (assert (= 3 (state-get-value (measure-machine))))))
        
