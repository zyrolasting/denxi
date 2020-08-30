#lang racket/base

(require "contract.rkt"
         "string.rkt")

(provide (struct-out output-info)
         (contract-out
          [well-formed-output-info/c
           flat-contract?]))

(struct output-info
  (name              ; The name of the link used to reference the output of `builder-name`
   build-expression) ; An expression to eval in a sandbox
  #:prefab)

(define well-formed-output-info/c
  (struct/c output-info
            non-empty-string?
            any/c))
