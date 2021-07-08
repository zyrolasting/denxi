#lang racket/base

(require racket/contract)

(provide transparent-package-output
         (struct-out package-output)
         (contract-out
          [encode-package-output
           (-> package-output?
               list?)]
          [find-package-output
           (-> string?
               (listof package-output?)
               (or/c #f package-output?))]))

(require (for-syntax racket/base
                     "string.rkt")
         syntax/parse/define
         "monad.rkt"
         "subprogram.rkt")

(struct package-output (name steps make-subprogram))

(define-syntax-parse-rule (transparent-package-output name:non-empty-string steps:expr ...)
  (package-output
   name
   (quote (mdo steps ...))
   (lambda () (coerce-subprogram (mdo steps ...)))))

(define (encode-package-output output)
  `(output ,(package-output-name output)
           . ,(package-output-steps output)))

(define (find-package-output name outputs)
  (findf (λ (o) (equal? name (package-output-name o)))
         outputs))
