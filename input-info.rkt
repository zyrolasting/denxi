#lang racket/base

(require "contract.rkt"
         "string.rkt"
         "source.rkt")

(provide (struct-out input-info)
         (contract-out
          [well-formed-input-info/c
           flat-contract?]))

(struct input-info
  (name        ; The name to bind to bytes
   fetch-info) ; Defines where said bytes come from
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            name-string?
            well-formed-fetch-info/c))
