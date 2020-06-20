#lang racket/base

; Defines a revision and related operations.  Revisions represent
; changes in an edition over time.  Group revisions in sequences, but
; allow a user to query revision ranges using friendly names.
; Protect the user from backwards intervals.

(require racket/contract
         racket/generator
         racket/match
         racket/sequence
         racket/string
         "base.rkt")

(provide (all-defined-out))

(struct revision (retracted? names))
(struct exn:fail:zcpkg:invalid-revision-interval exn:fail (lo hi))

; High-level resolution procedure. Uses the rest of the module.
(define (resolve-revision-query v revisions)
  (cond [(exact-nonnegative-integer? v)
         (sequence-ref revisions v)]
        [(revision-number-string? v)
         (resolve-revision-query (string->number v) revisions)]
        [(revision-name-string? v)
         (resolve-revision-query (revision-name->number v revisions) revisions)]
        [(revision-range-string? v)
         (resolve-revision-range v revisions)]
        [else (raise-argument-error
               'resolve-revision-query
               "A valid revision query"
               v)]))

; Iterates over available revisions with preference to later revisions.
(define (in-available-revisions revisions [lo 0] [hi (sub1 (sequence-length revisions))])
  (assert-valid-revision-range lo hi)
  (in-generator #:arity 2
    (for ([i (in-range hi (sub1 lo) -1)])
      (define rev (sequence-ref revisions i))
      (unless (revision-retracted? rev)
        (yield rev i)))))

; Get the latest in the range
(define (resolve-revision-range str revisions)
  (define-values (l h) (revision-range->values str revisions))
  (for/or ([(rev index) (in-available-revisions revisions l h)])
    rev))

; Translate "[initial, breaking-change)" to a numerical equivalent.
(define (revision-range->values str revisions)
  (match (parse-revision-range-string str)
    [(list _ open-bracket low-name hi-name close-bracket)

     (define low-num (revision-name->number low-name revisions))
     (define hi-num  (revision-name->number hi-name revisions))

     (define low-adjusted
       (if (equal? open-bracket "(")
           (add1 low-num) low-num))

     (define hi-adjusted
       (if (equal? close-bracket ")")
           (sub1 hi-num)
           hi-num))

     (assert-valid-revision-range low-adjusted hi-adjusted str)

     (values low-adjusted hi-adjusted)]

    [_ (raise-argument-error
        'resolve-revision-range
        "A valid revision range string"
        str)]))

(define (revision-name->number name revisions)
  (cond [(exact-nonnegative-integer? name) name]
        [(revision-number-string? name)
         (revision-name->number (string->number name) revisions)]
        [(equal? name "oldest") 0]
        [(equal? name "newest") (sub1 (sequence-length revisions))]
        [(string? name)
         (for/or ([(rev index) (in-available-revisions revisions)])
           (and (member name (revision-names rev))
                index))]
        [else #f]))

(define (assert-valid-revision-range lo hi [named-interval #f])
  (when (< hi lo)
    (raise (make-revision-interval-error lo hi named-interval))))

(define (make-revision-interval-error lo hi [named-interval #f])
  (exn:fail:zcpkg:invalid-revision-interval
   (if named-interval
       (format (string-append
                "~v makes invalid interval [~v, ~v]. "
                "If the names are correct, then are they reversed?")
               named-interval lo hi)
       (format "Cannot use revision interval [~v, ~v]: The upper bound is less than the lower bound."
               lo hi))
   (current-continuation-marks)
   lo
   hi))

(define revision-open-bracket-pattern-string  "[\\(\\[]")
(define revision-close-bracket-pattern-string "[\\)\\]]")

(define revision-range-pattern-string
  (string-append
   revision-open-bracket-pattern-string
   maybe-spaces-pattern-string
   name-pattern-string
   maybe-spaces-pattern-string
   ","
   maybe-spaces-pattern-string
   name-pattern-string
   maybe-spaces-pattern-string
   revision-close-bracket-pattern-string))

(define revision-number-pattern-string "\\d+")

(define revision-pattern-string
  (or/pattstr revision-number-pattern-string
              revision-range-pattern-string
              name-pattern-string))

(define revision-name-string?
  (make-rx-predicate name-pattern-string))

(define revision-number-string?
  (make-rx-predicate revision-number-pattern-string))

(define revision-range-string?
  (make-rx-predicate revision-range-pattern-string))

(define parse-revision-range-string
  (make-rx-matcher
   (string-append
    (group/pattstr revision-open-bracket-pattern-string)
    maybe-spaces-pattern-string
    (group/pattstr name-pattern-string)
    maybe-spaces-pattern-string
    ","
    maybe-spaces-pattern-string
    (group/pattstr name-pattern-string)
    maybe-spaces-pattern-string
    (group/pattstr revision-close-bracket-pattern-string))))


(define revision-string?
  (make-rx-predicate revision-pattern-string))

(define revision-query/c
  (or/c exact-nonnegative-integer? revision-string?))
