#lang racket/base

; Define a package output format and related operations. The main two
; being to declare a package output, and to run a program with needed
; inputs

(require racket/contract racket/string)

(provide (struct-out output-info)
         (contract-out
          [well-formed-output-info/c
           flat-contract?]))

(struct output-info
  (name
   builder-name
   builder-expressions)
  #:prefab)

(define well-formed-output-info/c
  (struct/c output-info
            non-empty-string?
            non-empty-string?
            list?))
