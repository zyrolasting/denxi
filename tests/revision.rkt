#lang racket/base

(require racket/format
         racket/generator
         racket/sequence
         racket/string
         idiocket/bool
         rackunit
         zcpkg/source/revision)

(define (make-valid-range-sequence low hi)
  (in-generator
   (yield (~a "(" low "," hi ")"))
   (yield (~a "[" low "," hi ")"))
   (yield (~a "[" low "," hi "]"))
   (yield (~a "(" low "," hi "]"))))

(define (test-true* p seq)
  (for ([args (in-values-sequence seq)])
    (test-true (format "Expect: (eq? #t (~a ~a))"
                       (object-name p)
                       (string-join (map ~v args)
                                    " "))
               (apply p args))))

(module+ test
  (test-true "Accept (large) positive integers"
             (revision-number-string? "87897679687236872363278692984"))
  (test-true "Accept \"0\""
             (revision-number-string? "0"))
  (test-false "Reject negative numbers"
              (revision-number-string? "-1"))
  (test-false "Reject floating-point numbers"
              (revision-number-string? "1.224"))
  (test-false "Reject expressions"
              (revision-number-string? "1/2"))

  (define valid-ranges
    (sequence-append (make-valid-range-sequence 0 0)
                     (make-valid-range-sequence "oldest" "something")))

  (test-true* revision-range-string?
              valid-ranges)

  (test-true* revision-string?
              (sequence-append valid-ranges
                               '("0"
                                 "oldest"
                                 "newest"
                                 "-chookies")))

  (test-case "Revision range operations"
    (define revisions
      (vector (revision #f '("initial"))
              (revision #f null)
              (revision #f '("after-patch" "new-beta"))
              (revision #f null)
              (revision #t '("retracted"))
              (revision #f '("latest"))))

    (test-eq? "Can look up a revision by name"
              (revision-name->number "initial" revisions)
              0)

    (test-eq? "\"oldest\" is reserved"
              (revision-name->number "oldest" revisions)
              0)

    (test-eq? "\"newest\" is reserved"
              (revision-name->number "newest" revisions)
              (sub1 (vector-length revisions)))

    (test-eq? "A revision can have multiple names"
               (revision-name->number "after-patch" revisions)
               (revision-name->number "new-beta" revisions))

    (test-eq? "One of a revision's names can be implicit"
              (revision-name->number "newest" revisions)
              (revision-name->number "latest" revisions))

    (test-eq? "Revision ranges can match only one revision"
      (resolve-revision-query "[after-patch, after-patch]" revisions)
      (resolve-revision-query "after-patch" revisions))

    (test-case "Revision ranges must translate to valid intervals"
      (define (check-interval str)
        (check-exn
         exn:fail:zcpkg:invalid-revision-interval?
         (λ () (resolve-revision-query str revisions))))
      (check-interval "[1, 1)")
      (check-interval "[1, 0]")
      (check-interval "[newest, oldest]")
      (check-interval "[new-beta, new-beta)"))))
