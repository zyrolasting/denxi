#lang racket/base

; Define a package output format and related operations. The main two
; being to declare a package output, and to run a program with needed
; inputs

(require racket/contract)
(provide (struct-out output-info)
         (contract-out
          [well-formed-output-info/c
           flat-contract?]
          [build-output!
           (-> well-formed-output-info/c
               complete-path?)]))


(require racket/string
         racket/system
         compiler/find-exe)

(struct output-info
  (name
   builder-name
   racket-arguments
   builder-arguments)
  #:prefab)


(define well-formed-output-info/c
  (struct/c output-info
            non-empty-string?
            non-empty-string?
            (listof string?)
            (listof string?)))

; Assumes current-directory is where the builder exists.
(define (build-output! info)
  (system* (find-exe)
           (append (output-info-racket-arguments info)
                   (cons (output-info-builder-name info)
                         (output-info-builder-arguments info)))))
