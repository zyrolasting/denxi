#lang racket/base

; Define a module language to express inputs

(require racket/contract
         racket/function
         racket/string
         "encode.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "signature.rkt")

(provide #%app
         #%datum
         #%module-begin
         #%top
         quote
         quasiquote
         unquote
         (contract-out
          [file
           (->* (non-empty-string?
                 (non-empty-listof string?))
                ((or/c #f integrity-info?)
                 (or/c #f signature-info?))
                input-info?)]

          [package
           (->* (non-empty-string?
                 (non-empty-listof non-empty-string?))
               ((or/c #f integrity-info?)
                (or/c #f signature-info?))
               input-info?)]

          [sources
           (->* () #:rest (non-empty-listof non-empty-string?)
                (non-empty-listof non-empty-string?))]

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

(define (file name sources [integrity #f] [signature #f])
  (input-info 'file
              name
              sources
              integrity
              signature))

(define (package name sources [integrity #f] [signature #f])
  (input-info 'package
              name
              sources
              integrity
              signature))

(define sources list)
(define integrity integrity-info)
(define signature signature-info)
(define fingerprint values)
(define base32 (curry decode 'base32))
(define base64 (curry decode 'base64))
