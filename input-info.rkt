#lang racket/base

(require "contract.rkt"
         "integrity.rkt"
         "signature.rkt"
         "string.rkt"
         "source.rkt")

(provide (struct-out input-info)
         (contract-out
          [well-formed-input-info/c
           flat-contract?]))

(struct input-info
  (name       ; The name to bind to bytes
   sources    ; Defines where said bytes come from
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            name-string?
            well-formed-fetch-info/c))
