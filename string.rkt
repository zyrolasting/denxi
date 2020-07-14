#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/string
                       racket/format))

(require racket/string
         racket/format)

(define (whole/pattstr s) (string-append "^" s "$"))
(define (group/pattstr s)
  (string-append "(" s ")"))
(define (or/pattstr . opts)
  (string-append "(?:"
                 (string-join opts "|")
                 ")"))

(define (~a* . args)
  (apply ~a (map (λ (s) (~a s "\n")) args)))

; TODO: Flesh out definition of allowed names.
;
; - No spaces
; - No character or word that a Windows, Mac OSX, or a (typical) Linux distribution would reserve.
; - No path separators
;
(define name-pattern-string "[^/\\s]+")
(define maybe-spaces-pattern-string "\\s*")

(define (make-rx-predicate p #:whole? [whole? #t])
  (define rx (pregexp (if whole? (whole/pattstr p) p)))
  (λ (v)
    (and (string? v)
         (regexp-match? rx v))))

(define (make-rx-matcher p #:whole? [whole? #t])
  (define rx (pregexp (if whole? (whole/pattstr p) p)))
  (λ (v)
    (and (string? v)
         (regexp-match rx v))))

(define name-string?
  (procedure-rename (make-rx-predicate name-pattern-string)
                    'name-string?))
