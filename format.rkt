#lang racket/base


(provide (all-defined-out)
         (all-from-out racket/format))

(require racket/format
         racket/string)

(define (~a* . args)
  (apply ~a (map (λ (s) (~a s "\n")) args)))

(define (indent-lines lines)
  (map (λ (s) (~a "  " s)) lines))

(define (join-lines lines)
  (string-join lines "\n"))
