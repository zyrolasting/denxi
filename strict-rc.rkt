#lang racket/base

; A limited front end for rc.rkt that allows use of the runtime
; configuration without risk of cyclic dependencies.

(require racket/contract)

(provide
 (contract-out [rc-key? predicate/c]
               [rc-ref
                (-> rc-key? any/c)]
               [rc-rebind
                (-> rc-key? any/c (-> any) any)]))

(define (from-rc k)
  (dynamic-require 'xiden/rc k))

(define (rc-key? v)
  (hash-has-key? (from-rc 'XIDEN_SETTINGS) v))

(define (rc-get-setting k)
  (hash-ref (from-rc 'XIDEN_SETTINGS) k))

(define (rc-ref k)
  ((rc-get-setting k)))

(define (rc-rebind k v continue)
  ((rc-get-setting k) v continue))
