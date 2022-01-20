#lang racket/base

(require racket/contract)
(provide state?
         (contract-out
          [state-contents
           (-> state? stream?)]
          [state-add
           (-> state? source? transfer-policy? void?)]
          [state-ref
           (-> state? any/c (-> any/c) void?)]
          [state-link
           (-> state? any/c any/c void?)]
          [state-remove
           (-> state? any/c void?)]
          [state-unreachable
           (-> state? stream?)]
          [state-clean
           (-> state? stream?)]))


(define-message $gc (key bytes-recovered))


(require racket/function
         racket/generator
         racket/generic
         racket/stream
         "port.rkt"
         "source.rkt")


(define-generics state
  [state-contents state]
  [state-add      state source policy]
  [state-ref      state key/location failure-result]
  [state-link     state key location]
  [state-remove   state key])


(define state-unreachable
  (case-lambda
    [(st) (state-unreachable (state-contents st) empty-stream)]
    [(i o)
     (cond [(stream-empty? i) o]
           [(stream-empty? (stream-rest i))
            (stream-cons (let-values ([(k v) (stream-first i)]) k) o)]
           [else
            (state-unreachable (stream-rest i) o)])]))


(define (state-clean st)
  (stream-map
   (λ (k) ($gc k (state-remove st k)))
   (state-unreachable st)))
