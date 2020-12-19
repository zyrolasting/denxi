#lang racket/base

; Define and compare Racket version intervals


;-------------------------------------------------------------------------------
; Basic definitions

(define (unbounded? v)
  (equal? v UNBOUNDED_RACKET_VERSION))

(define UNBOUNDED_RACKET_VERSION "*")
(define PRESUMED_MINIMUM_RACKET_VERSION "0.0")
(define PRESUMED_MAXIMUM_RACKET_VERSION "9.99.999.999")

(define maybe-racket-version/c
  (or/c unbounded? valid-version?))

(define current-racket-version-relationship/c
  (or/c 'supported 'unsupported 'undeclared))

(define racket-version-range/c
  (or/c valid-version?
        (cons/c maybe-racket-version/c
                maybe-racket-version/c)))

(define racket-version-ranges/c
  (listof racket-version-range/c))


(require racket/contract)
(provide racket-version-selection
         (contract-out
          [UNBOUNDED_RACKET_VERSION non-empty-string?]
          [PRESUMED_MINIMUM_RACKET_VERSION valid-version?]
          [PRESUMED_MAXIMUM_RACKET_VERSION valid-version?]
          [racket-version-range/c flat-contract?]
          [racket-version-ranges/c flat-contract?]
          [unbounded? predicate/c]
          [make-racket-version-interval
           (-> maybe-racket-version/c
               maybe-racket-version/c
               (values exact-integer? exact-integer?))]
          [racket-version-in-range?
           (-> valid-version?
               maybe-racket-version/c
               maybe-racket-version/c
               boolean?)]
          [check-racket-version-ranges
           (-> valid-version?
               racket-version-ranges/c
               current-racket-version-relationship/c)]))


(require syntax/parse
         version/utils
         "exn.rkt"
         "l10n.rkt"
         "string.rkt")


(define-exn exn:fail:xiden:invalid-racket-version-interval exn:fail:xiden (lo hi))

(define (make-racket-version-interval min-v max-v)
  (define lo (version->integer (normalize-minimum-version min-v)))
  (define hi (version->integer (normalize-maximum-version max-v)))
  (if (< hi lo)
      (raise (exn:fail:xiden:invalid-racket-version-interval
              (format "Cannot match Racket version in reversed interval: [~a, ~a]" min-v max-v)
              (current-continuation-marks) min-v max-v))
      (values lo hi)))

(define (normalize-minimum-version v)
  (if (unbounded? v)
      PRESUMED_MINIMUM_RACKET_VERSION
      v))

(define (normalize-maximum-version v)
  (if (unbounded? v)
      PRESUMED_MAXIMUM_RACKET_VERSION
      v))

(define (racket-version-in-range? given-v min-v max-v)
  (define-values (lo hi) (make-racket-version-interval min-v max-v))
  (define v (version->integer given-v))
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


;-------------------------------------------------------------------------------
; Syntax classes for expressing version sets

(define-syntax-class racket-version-or-*
  (pattern (~var bound string)
           #:when (let ([v (syntax-e #'bound)])
                    (or (unbounded? v)
                        (valid-version? v)))))

(define-syntax-class racket-version-selection
  #:attributes (min max)
  (pattern (~and (min-variant:racket-version-or-* max-variant:racket-version-or-*)
                 (~bind [min (datum->syntax #'min-variant (normalize-minimum-version (syntax-e #'min-variant)))]
                        [max (datum->syntax #'max-variant (normalize-maximum-version (syntax-e #'max-variant)))])
                 (~fail #:unless (version<=? (syntax-e #'min) (syntax-e #'max))
                        (get-localized-string 'backwards-racket-version-interval))))
  (pattern (~and (~var v racket-version-or-*)
                 (~bind [min (datum->syntax #'max-variant (normalize-minimum-version (syntax-e #'v)))]
                        [max (datum->syntax #'max-variant (normalize-maximum-version (syntax-e #'v)))]))))



(module+ test
  (require rackunit)

  (define (test-invalid-interval min-v max-v)
    (test-exn (format "Flag [~a, ~a] as an invalid interval" min-v max-v)
            (λ (e)
              (and (exn:fail:xiden:invalid-racket-version-interval? e)
                   (check-eq? (exn:fail:xiden:invalid-racket-version-interval-lo e) min-v)
                   (check-eq? (exn:fail:xiden:invalid-racket-version-interval-hi e) max-v)))
            (λ ()
              (racket-version-in-range? "0.0" min-v max-v))))

  (for ([v (in-list (list null
                          `((,UNBOUNDED_RACKET_VERSION . ,UNBOUNDED_RACKET_VERSION))
                          `((,UNBOUNDED_RACKET_VERSION . "9.9") "6.3")
                          '(("0.0" . "7.7.0.5"))))])
    (test-true (format "Allow ~s as racket-version-ranges/c" v)
               (racket-version-ranges/c v)))

  (for ([v (in-list (list `(,UNBOUNDED_RACKET_VERSION . ,UNBOUNDED_RACKET_VERSION)
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
             (and (racket-version-in-range? "0.0" UNBOUNDED_RACKET_VERSION "8.0")
                  (racket-version-in-range? "1.0" UNBOUNDED_RACKET_VERSION "8.0")
                  (not (racket-version-in-range? "8.1" UNBOUNDED_RACKET_VERSION "8.0"))))

  (test-true "Check unbounded maximum"
             (and (not (racket-version-in-range? "1.0" "3.0" UNBOUNDED_RACKET_VERSION))
                  (racket-version-in-range? "9.99" "3.0" UNBOUNDED_RACKET_VERSION)))



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
                                         `(("0.1" . "0.2")
                                           (,UNBOUNDED_RACKET_VERSION . ,UNBOUNDED_RACKET_VERSION)))
            'supported)

  (test-eq? "Flag any version matching no range as unsupported"
            (check-racket-version-ranges "1.2"
                                         '(("0.1" . "0.2")))
            'unsupported)

  (test-true "Detect valid Racket versions in a macro"
             (syntax-parse #'"7.7.0.5" [v:racket-version-or-* #t] [_ #f]))

  (test-true "Allow unbounded versions"
             (syntax-parse #'"*" [v:racket-version-or-* #t] [_ #f]))

  (test-equal? "Express version intervals"
               (syntax-parse #'("*" ("*" "*") "6.5" ("9.1" "9.3.4"))
                 [(v:racket-version-selection ...+)
                  (syntax->datum #'((v.min v.max) ...))]
                 [_ #f])
               `((,PRESUMED_MINIMUM_RACKET_VERSION ,PRESUMED_MAXIMUM_RACKET_VERSION)
                 (,PRESUMED_MINIMUM_RACKET_VERSION ,PRESUMED_MAXIMUM_RACKET_VERSION)
                 ("6.5" "6.5")
                 ("9.1" "9.3.4")))

  (test-exn "Capture backwards version intervals during expansion"
            exn:fail:syntax?
            (λ ()
              (syntax-parse #'("9.3.4" "9.1")
                [v:racket-version-selection #f]))))
