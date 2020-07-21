#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/string))

(require racket/string)

(define (whole/pattstr s) (string-append "^" s "$"))
(define (group/pattstr s)
  (string-append "(" s ")"))
(define (or/pattstr . opts)
  (string-append "(?:"
                 (string-join opts "|")
                 ")"))

(define name-pattern-string "[^/\\\\:\\s]+")
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

(module+ test
  (require rackunit)

  (check-false (name-string? "/"))
  (check-false (name-string? "\\"))
  (check-false (name-string? ":"))
  (check-false (name-string? " "))
  (check-false (name-string? "alvin:bet"))
  (check-pred name-string? "alvin")

  (let ([three-digits^$ (make-rx-predicate "\\d\\d\\d")]
        [three-digits (make-rx-predicate "\\d\\d\\d" #:whole? #f)])
    (test-case "Match whole pattern by predicate"
      (check-true (three-digits^$ "002"))
      (check-false (three-digits^$ " 002"))
      (check-true (three-digits " 002"))))

  (let ([choice (make-rx-predicate (or/pattstr "alpha" "beta") #:whole? #f)])
    (check-true (choice "xxx alpha yyy"))
    (check-true (choice "xxx beta yyy")))

  (let ([capt (make-rx-matcher #:whole? #f
                               (string-append
                                (group/pattstr (or/pattstr "\\d\\d" "[a-f][a-f]"))
                                "x"
                                (group/pattstr (or/pattstr "\\d\\d" "[a-f][a-f]"))))])
    (check-equal? (capt "dax29")
                  (list "dax29" "da" "29"))))
