#lang racket/base

(require racket/contract
         version/utils)

(define maybe-version/c
  (or/c #f valid-version?))

(define current-racket-version-relationship/c
  (or/c 'supported 'unsupported 'undeclared))

(define racket-version-ranges/c
  (listof (or/c valid-version?
                (cons/c maybe-version/c
                        maybe-version/c))))

(provide
 (struct-out exn:fail:zcpkg:invalid-racket-version-interval)
 (contract-out
  [PRESUMED_MINIMUM_RACKET_VERSION valid-version?]
  [PRESUMED_MAXIMUM_RACKET_VERSION valid-version?]
  [racket-version-ranges/c flat-contract?]
  [racket-version-in-range?
   (-> valid-version?
       maybe-version/c
       maybe-version/c
       boolean?)]
  [check-racket-version-ranges
   (-> valid-version?
       racket-version-ranges/c
       current-racket-version-relationship/c)]))

(struct exn:fail:zcpkg:invalid-racket-version-interval exn:fail (lo hi))

(define PRESUMED_MINIMUM_RACKET_VERSION "0.0")
(define PRESUMED_MAXIMUM_RACKET_VERSION "9.99.999.999")

(define (racket-version-in-range? given-v min-v max-v)
  (define lo (version->integer (or min-v PRESUMED_MINIMUM_RACKET_VERSION)))
  (define hi (version->integer (or max-v PRESUMED_MAXIMUM_RACKET_VERSION)))
  (define v  (version->integer given-v))

  (when (< hi lo)
    (raise (exn:fail:zcpkg:invalid-racket-version-interval
            (format "Cannot match Racket version in reversed interval: [~a, ~a]" min-v max-v)
            (current-continuation-marks) min-v max-v)))

  (and (<= lo v)
       (<= v hi)))


(define (check-racket-version-ranges v ranges)
  (if (null? ranges)
      'undeclared
      (if (for/or ([variant (in-list ranges)])
            (define pair
              (if (pair? variant)
                  variant
                  (cons variant variant)))
            (racket-version-in-range? v (car pair) (cdr pair)))
          'supported
          'unsupported)))


(module+ test
  (require rackunit)

  (define (test-invalid-interval min-v max-v)
    (test-exn (format "Flag [~a, ~a] as an invalid interval" min-v max-v)
            (λ (e)
              (and (exn:fail:zcpkg:invalid-racket-version-interval? e)
                   (check-eq? (exn:fail:zcpkg:invalid-racket-version-interval-lo e) min-v)
                   (check-eq? (exn:fail:zcpkg:invalid-racket-version-interval-hi e) max-v)))
            (λ ()
              (racket-version-in-range? "0.0" min-v max-v))))

  (for ([v (in-list (list null
                          '((#f . #f))
                          '((#f . "9.9") "6.3")
                          '(("0.0" . "7.7.0.5"))))])
    (test-true (format "Allow ~s as racket-version-ranges/c" v)
               (racket-version-ranges/c v)))

  (for ([v (in-list (list '(#f . #f)
                          "0.0"
                          '(())
                          '(("999.999" . "0.0"))))])
    (test-false (format "Forbid ~s as racket-version-ranges/c" v)
                (racket-version-ranges/c v)))

  (test-true "Check (inclusive) minimum"
             (racket-version-in-range? "7.7.0.5" "7.7.0.5" "8.0"))

  (test-true "Check (inclusive) maximum"
             (racket-version-in-range? "8.0" "7.7.0.5" "8.0"))

  (test-true "Check exact"
             (racket-version-in-range? "7.7.0.5" "7.7.0.5" "7.7.0.5"))

  (test-true "Check unbounded minimum"
             (and (racket-version-in-range? "0.0" #f "8.0")
                  (racket-version-in-range? "1.0" #f "8.0")
                  (not (racket-version-in-range? "8.1" #f "8.0"))))

  (test-true "Check unbounded maximum"
             (and (not (racket-version-in-range? "1.0" "3.0" #f))
                  (racket-version-in-range? "9.99" "3.0" #f)))



  ; Only one case works here since we're not offering exclusive endpoints
  (test-invalid-interval "1.0" "0.0")

  (test-eq? "Detect undeclared version ranges"
            (check-racket-version-ranges (version) null)
            'undeclared)

  (test-eq? "Match a version against any one range"
            (check-racket-version-ranges "1.2"
                                         '(("0.5" . "0.8")
                                           ("1.0" . "2.0")))
            'supported)

  (test-eq? "Match a version against any one range"
            (check-racket-version-ranges "1.2"
                                         '(("0.5" . "0.8")
                                           ("1.0" . "2.0")))
            'supported)

  (test-eq? "Match an exact version among ranges"
            (check-racket-version-ranges "1.2"
                                         '(("0.5" . "0.8") "1.2"))
            'supported)

  (test-eq? "An unbounded range anywhere is equivalent to matching every version"
            (check-racket-version-ranges "1.2"
                                         '(("0.1" . "0.2")
                                           (#f . #f)))
            'supported)

  (test-eq? "Flag any version matching no range as unsupported"
            (check-racket-version-ranges "1.2"
                                         '(("0.1" . "0.2")))
            'unsupported))
