#lang racket/base

(require "contract.rkt"
         "integrity.rkt"
         "signature.rkt"
         "string.rkt")

(provide (struct-out input-info)
         (contract-out
          [well-formed-input-info/c
           flat-contract?]))

(struct input-info
  (name       ; The name of the link used to reference input bytes
   sources    ; Where to look to get bytes
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            non-empty-string?
            (non-empty-listof any/c)
            (or/c #f well-formed-integrity-info/c)
            (or/c #f well-formed-signature-info/c)))
