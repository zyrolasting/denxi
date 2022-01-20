#lang racket/base

(require racket/contract)
(provide state?
         (contract-out
          [current-state
           (parameter/c state?)]
          [state-contents
           (-> state? (stream/c known?))]
          [state-add
           (-> state? known? (subprogram/c void?))]
          [state-ref
           (-> state? any/c (-> any/c) stream?)]
          [state-remove
           (-> state? any/c void?)]
          [state-unreachable
           (-> state? stream?)]
          [state-clean
           (-> state? stream?)]))

(require racket/function
         racket/generator
         racket/generic
         racket/stream
         "known.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt"
         "source.rkt"
         "subprogram.rkt")


(define+provide-message $gc (key bytes-recovered))


(define-generics state
  [state-contents state]
  [state-add state known]
  [state-ref state key fail]
  [state-remove state key])


(define current-state
  (make-parameter #f))


#;(define (persist-source state source known)
  (mdo f := (known-open-output known)
       (subprogram
        (λ (messages)
          (let ([canonical-name (make-source-key source)])
            (known-forget-names known)
            (known-add-name canonical-name)
            (state-add canonical-name known)
            (subprogram-fetch canonical-name source
                              (λ (est-size in)
                                (f in
                                   (struct-copy transfer-policy
                                                zero-trust-transfer-policy
                                                [max-size MAX_EXPECTED_DIGEST_LENGTH]
                                                [est-size est-size]
                                                [buffer-size MAX_EXPECTED_DIGEST_LENGTH]
                                                [timeout-ms (DENXI_FETCH_TIMEOUT_MS)]
                                                [telemeter void])))))))))


(define state-unreachable
  (case-lambda
    [(st) (state-unreachable (state-contents st) empty-stream)]
    [(i o)
     (if (stream-empty? i)
         o
         (let-values ([(k v) (stream-first i)])
           (state-unreachable (stream-rest i)
                              (if (known-here? v)
                                  o
                                  (stream-cons k o)))))]))


(define (state-clean st)
  (stream-map
   (λ (k) ($gc k (state-remove st k)))
   (state-unreachable st)))


(module+ test
  (require rackunit)
  (provide test-state)
  (define-syntax-rule (test-state . xs)
    (error "implement test-state")))
