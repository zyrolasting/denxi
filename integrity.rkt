#lang racket/base


(require racket/contract
         "crypto.rkt"
         "machine.rkt"
         "message.rkt"
         "port.rkt")

(provide (struct-out $integrity)
         (struct-out integrity)
         (contract-out
          [integrity-check
           (-> string?
               bytes?
               integrity-policy?
               input-port?
               (machine/c boolean?))]
          [chf/c chaperone-contract?]))


(define-message $integrity (name status))


; A claim regarding data integrity
(struct integrity (chf-name digest))


(define chf/c (-> input-port? bytes?))


(define-syntax-rule (chf-canon [prescription experience chf] ...)
  (dynamic-chf-canon (make-immutable-hash (list (cons prescription chf) ...))
                     (canon [prescription experience] ...)))


(define ((dynamic-chf-canon table canonicalize) utterance)
  (hash-ref table (canonicalize utterance) #f))


(define (integrity-check canon chf-name trusted-digest in)
  (machine
   (λ (state)
     (let/ec escape
       (define (return pass? status)
         (define message ($integrity chf-name status))
         (define state* (state-add-message state message))
         (define state** (state-set-value state* pass?))
         (escape state**))
       (define chf (canon policy chf-name))
       (unless chf (return #f 0))
       (if (equal? trusted-digest (chf in))
           (return #t 1)
           (return #f 2))))))


(define (integrity-check-passed? state)
  (eq? #t (state-get-value state)))


(module+ test
  (require racket/file
           racket/function
           rackunit
           "codec.rkt"
           "crypto.rkt")

  (define content #"abc")
  (define digest #"\251\231>6G\6\201j\272>%qxP\302l\234\320\330\235"))
