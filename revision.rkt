#lang racket/base

; Defines a revision and related operations.  Revisions represent
; changes in an edition over time.  Group revisions in sequences, but
; allow a user to query revision ranges using friendly names.
; Protect the user from backwards intervals.

(require racket/contract
         racket/generator
         racket/match
         racket/sequence
         "string.rkt")

(provide (all-defined-out))

(struct revision (retracted? names))
(struct exn:fail:zcpkg:invalid-revision-interval exn:fail (lo hi))

(define (add1-if do-it? low-num)
  (if do-it? (add1 low-num) low-num))

(define (sub1-if do-it? hi-num)
  (if do-it? (sub1 hi-num) hi-num))

; High-level resolution procedure. Uses the rest of the module.
(define (resolve-revision-query v revisions)
  (define-values (l h) (revision-query->values v revisions))
  (for/or ([(rev index) (in-available-revisions revisions l h)])
    rev))

(define (revision-query->values v revisions)
  (cond [(revision-number? v) v]
        [(revision-number-string? v)
         (revision-query->values (string->number v) revisions)]
        [(revision-range-string? v)
         (revision-range->values v revisions)]
        [(revision-name-string? v)
         (revision-query->values (revision-name->number v revisions) revisions)]
        [else (raise-argument-error
               'revision-query->values
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

(define (revision-number-in-range? n l h)
  (and (>= n l)
       (<= n h)))

(define (revision-range-subset? l1 h1 l2 h2)
  (and (revision-number-in-range? l1 l2 h2)
       (revision-number-in-range? h1 l2 h2)))

; Translate "[initial, breaking-change)" to a numerical equivalent.
(define (revision-range->values str revisions)
  (match (parse-revision-range-string str)
    [(list _ open-bracket low-name hi-name close-bracket)

     (define low-num (revision-name->number low-name revisions))
     (define hi-num  (revision-name->number hi-name revisions))

     (define low-adjusted
       (add1-if (equal? open-bracket "(") low-num))

     (define hi-adjusted
       (sub1-if (equal? close-bracket ")") hi-num))

     (assert-valid-revision-range low-adjusted hi-adjusted str)

     (values low-adjusted hi-adjusted)]

    [_ (raise-argument-error
        'resolve-revision-range
        "A valid revision range string"
        str)]))

(define (revision-name->number name revisions)
  (cond [(revision-number? name) name]
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

(define revision-number?
  exact-nonnegative-integer?)

(define revision-query/c
  (or/c revision-number? revision-string?))
