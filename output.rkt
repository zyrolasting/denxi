#lang racket/base

(require racket/contract)

(provide (struct-out package-output)
         (contract-out
          [encode-package-output
           (-> package-output?
               list?)]
          [find-package-output
           (-> string?
               (listof package-output?)
               (or/c #f package-output?))]))

(struct package-output (name steps make-subprogram))

(define (encode-package-output output)
  `(output ,(package-output-name output)
           . ,(package-output-steps output)))

(define (find-package-output name outputs)
  (findf (λ (o) (equal? name (package-output-name o)))
         outputs))
