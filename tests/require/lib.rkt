#lang racket/base

(provide test-require-spec
         test-require-spec/missing
         (all-from-out zcpkg/require
                       rackunit))

(require zcpkg/require
         syntax/macro-testing
         rackunit
         (for-syntax racket/base))

(define-syntax (test-require-spec stx)
  (define rewrite
    (syntax-case stx ()
      [(_ path expected)
       #'(let ()
           (local-require (zcp path))
           (check-equal? data expected))]))
  (datum->syntax stx (syntax->datum rewrite)))

(define-syntax-rule (test-require-spec/missing apkg-path)
  (test-exn (format "Should not find ~a" 'apkg-path)
            #rx"not found"
            (λ ()
              (convert-compile-time-error
               (let () (test-require-spec apkg-path #f))))))
