#lang racket/base

; Define a module language for non-trivial expressions inside package
; definitions.

(require racket/contract
         racket/function
         racket/string
         "encode.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "output-info.rkt"
         "signature.rkt")

(provide #%app
         #%datum
         #%module-begin
         #%top
         quote
         quasiquote
         unquote
         (rename-out [list sources])
         (contract-out
          [input
           (->* (non-empty-string?
                 (non-empty-listof string?))
                ((or/c #f integrity-info?)
                 (or/c #f signature-info?))
                input-info?)]

          [output
           (->* (non-empty-string?
                 (non-empty-listof string?))
                (list?)
                output-info?)]

          [integrity
           (-> xiden-hash-algorithm/c
               bytes?
               integrity-info?)]

          [signature
           (-> xiden-cipher-algorithm/c
               (or/c bytes? path-string?)
               bytes?
               signature-info?)]

          [fingerprint
           (-> (or/c non-empty-string? bytes?) bytes?)]

          [base32
           (-> (or/c non-empty-string? bytes?) bytes?)]

          [base64
           (-> (or/c non-empty-string? bytes?) bytes?)]))

(define (input name sources [integrity #f] [signature #f])
  (input-info name
              sources
              integrity
              signature))

(define (output name builder-name [exprs null])
  (output-info name
               builder-name
               exprs))

(define integrity integrity-info)
(define signature signature-info)
(define fingerprint values)
(define base32 (curry decode 'base32))
(define base64 (curry decode 'base64))