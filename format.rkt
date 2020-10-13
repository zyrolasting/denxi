#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/format))

(require racket/format
         racket/string)

(define (format-symbol-for-message i)
  (format "`~a`" i))

(define (indent-lines lines)
  (map (λ (s) (~a "  " s)) lines))

(define (join-lines lines)
  (string-join lines "\n"))

(define (join-lines* . lines)
  (join-lines lines))
