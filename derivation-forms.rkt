#lang racket/base

; Define a module language for derivations

(provide λ
         #%app
         #%datum
         #%module-begin
         #%top
         #%top-interaction
         define
         error
         eval
         fetch-input
         hash
         hash-ref
         lambda
         let
         quote
         quasiquote
         unquote)

(require racket/contract
         "archiving.rkt"
         "encode.rkt"
         "compression.rkt"
         "localstate.rkt"
         "message.rkt"
         "rc.rkt"
         "source.rkt")

(define (fetch-input info)
  (fetch info make-addressable-file))
