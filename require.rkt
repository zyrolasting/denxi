#lang racket/base

; Provide a require transformer.
; It must search for AP modules using identifiers.

(provide alib)

(require (for-syntax racket/base
                     racket/path
                     racket/require-syntax
                     racket/require-transform
                     syntax/location
                     syntax/parse
                     "path.rkt"))

(define-syntax alib
  (make-require-transformer
   (λ (stx)
     (syntax-parse stx
       [(_ pkg-path:id ...+)
        (let ([dir (or (syntax-source-directory stx) (current-directory))])
          (expand-import
           (datum->syntax stx
            `(combine-in
              . ,(map (λ (n)
                        (define maybe-path
                          (resolve-ap-path (build-path (symbol->string (syntax-e n))) dir))
                        (unless maybe-path
                          (raise-syntax-error 'alib "Assimilable module not found" n))
                        `(file ,(path->string maybe-path)))
                      (syntax->list #'(pkg-path ...)))))))]))))
