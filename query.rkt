#lang racket/base

; Define a data type to match against package definitions.

(require racket/function
         racket/generic
         racket/match
         "contract.rkt"
         "format.rkt"
         "logged.rkt"
         "message.rkt"
         "string.rkt"
         "version.rkt")

(provide (struct-out parsed-package-query)
         gen:package-query-defaults
         package-query-defaults?
         package-query-defaults-implementation/c
         gen:package-query-canon
         package-query-canon?
         package-query-canon-implementation/c
         (contract-out
          [default-package-query-defaults
           package-query-defaults-implementation/c]
          [boundary-flags-string? predicate/c]
          [well-formed-package-query? predicate/c]
          [resolved-package-query? predicate/c]
          [malformed-package-query? predicate/c]
          [exact-package-query? predicate/c]
          [package-query? predicate/c]
          [package-query-variant? predicate/c]
          [coerce-parsed-package-query (-> package-query-variant? parsed-package-query?)]
          [format-parsed-package-query (-> well-formed-package-query? string?)]
          [abbreviate-exact-package-query (-> exact-package-query? string?)]
          [make-exact-package-query
           (-> string? string? string? revision-number? exact-package-query?)]
          [parse-package-query (-> package-query? parsed-package-query?)]
          [resolve-minimum-revision (-> parsed-package-query? (-> string? any/c) any/c)]
          [resolve-maximum-revision (-> parsed-package-query? (-> string? any/c) any/c)]
          [autocomplete-parsed-package-query
           (-> package-query-defaults-implementation/c
               parsed-package-query?
               well-formed-package-query?)]
          [get-default-provider
           (-> package-query-defaults? string?)]
          [get-default-package
           (-> package-query-defaults? string? string?)]
          [get-default-edition
           (-> package-query-defaults? string? string? string?)]
          [get-default-min-revision
           (-> package-query-defaults? string? string? string? string?)]
          [get-default-max-revision
           (-> package-query-defaults? string? string? string? string? string?)]
          [get-default-interval-bounds
           (-> package-query-defaults? (or/c "ii" "ie" "ei" "ee"))]
          [make-canonical-package-query
           (->* (package-query-canon-implementation/c
                 package-query-defaults-implementation/c
                 package-query-variant?)
                (#:force-complete-interval? any/c)
                (logged/c exact-package-query?))]
          [find-revision-number
           (-> package-query-canon?
               string?
               string?
               string?
               string?
               any/c)]
          [select-revision-number
           (-> package-query-canon?
               string?
               string?
               string?
               revision-number?
               revision-number?
               any/c)]))


(define+provide-message $package-query-canon (user-query autocompleted-query))
(define+provide-message $package-query-canon:backwards  $package-query-canon (alleged-minimum alleged-maximum))
(define+provide-message $package-query-canon:no-minimum $package-query-canon (hint))
(define+provide-message $package-query-canon:no-maximum $package-query-canon (hint))
(define+provide-message $package-query-canon:no-selection $package-query-canon (minimum maximum hint))
(define+provide-message $package-query-canon:oob $package-query-canon (minimum maximum selection))


(struct parsed-package-query
  (provider-name
   package-name
   edition-name
   revision-min
   revision-max
   interval-bounds)
  #:transparent)


(define (boundary-flags-string? s)
  (with-handlers ([exn? (λ (e) #f)])
    (boundary-flag->boolean (string-ref s 0))
    (boundary-flag->boolean (string-ref s 1))
    #t))


(define well-formed-package-query?
  (struct/c parsed-package-query
            string?
            string?
            string?
            string?
            string?
            (or/c "" boundary-flags-string?)))

(define malformed-package-query?
  (negate well-formed-package-query?))


(define (resolved-package-query? v)
  (and (well-formed-package-query? v)
       (revision-number-string? (parsed-package-query-revision-min v))
       (revision-number-string? (parsed-package-query-revision-max v))))


(define (exact-package-query? v)
  (and (resolved-package-query? v)
       (equal? (parsed-package-query-revision-min v)
               (parsed-package-query-revision-max v))
       (equal? (parsed-package-query-interval-bounds v)
               "ii")))


(define (package-query-variant? v)
  ((disjoin package-query? parsed-package-query?) v))


(define (coerce-parsed-package-query v)
  (cond [(parsed-package-query? v) v]
        [(package-query? v) (parse-package-query v)]))


(define (query-ref s def)
  (if (non-empty-string? s)
      s
      def))


(define (package-query? s)
  (with-handlers ([values (const #f)])
    (well-formed-package-query? (parse-package-query s))))


(define (parse-package-query s)
  (define user-defined (string-split #:trim? #f #:repeat? #f s ":"))
  (define num-fields (length user-defined))
  (apply parsed-package-query
         (build-list (procedure-arity parsed-package-query)
                     (λ (i)
                       (if (< i num-fields)
                           (list-ref user-defined i)
                           "")))))


(define (format-parsed-package-query d)
  (regexp-replace* ":+$"
                   (string-join (cdr (vector->list (struct->vector d)))
                                ":")
                   ""))


(define (abbreviate-exact-package-query q)
  (string-join (map (λ (acc) (acc q))
                    (list parsed-package-query-provider-name
                          parsed-package-query-package-name
                          parsed-package-query-edition-name
                          parsed-package-query-revision-min))
               ":"))


(define (boundary-flag->boolean flag)
  (case flag
    [(#\i) #f]
    [(#\e) #t]
    [else (raise-argument-error 'boundary-flag->boolean
                                "#\\i or #\\e"
                                flag)]))

(define (boolean->boundary-flag flag)
  (if flag #\e #\i))


(define (resolve-revision query accessor find-revision-number boundary-index continue)
  (define (on-success v)
    (continue (coerce-revision-number v)
              (boundary-flag->boolean
               (string-ref (parsed-package-query-interval-bounds query)
                           boundary-index))))

  (let ([revision (accessor query)])
    (if (revision-number-variant? revision)
        (on-success revision)
        (let ([value (with-handlers ([(negate exn:break?) values])
                       (find-revision-number revision))])
          (if (revision-number-variant? value)
              (on-success value)
              value)))))


(define (resolve-minimum-revision query find-revision-number)
  (resolve-revision query
                    parsed-package-query-revision-min
                    find-revision-number
                    0
                    (λ (num exclusive?)
                      (make-minimum-revision-number #:exclusive? exclusive? num))))

(define (resolve-maximum-revision query find-revision-number)
  (resolve-revision query
                    parsed-package-query-revision-max
                    find-revision-number
                    1
                    (λ (num exclusive?)
                      (make-maximum-revision-number #:exclusive? exclusive? num))))


(define (make-exact-package-query provider name edition revision-number)
  (let ([rn (~a revision-number)])
    (parsed-package-query provider
                          name
                          edition
                          rn
                          rn
                          "ii")))


(define-generics package-query-defaults
  [get-default-provider package-query-defaults]
  [get-default-package package-query-defaults provider]
  [get-default-edition package-query-defaults provider package]
  [get-default-min-revision package-query-defaults provider package edition]
  [get-default-max-revision package-query-defaults provider package edition min-revision]
  [get-default-interval-bounds package-query-defaults]
  #:fallbacks
  [(define (get-default-provider c) DEFAULT_STRING)
   (define (get-default-package c p) DEFAULT_STRING)
   (define (get-default-edition c p n) DEFAULT_STRING)
   (define (get-default-min-revision c p n e) "0")
   (define (get-default-max-revision c p n e m) DEFAULT_STRING)
   (define (get-default-interval-bounds c) "ii")])


(define package-query-defaults-implementation/c
  (package-query-defaults/c
   [get-default-provider
    (or/c #f (-> package-query-defaults? string?))]
   [get-default-package
    (or/c #f (-> package-query-defaults? string? string?))]
   [get-default-edition
    (or/c #f (-> package-query-defaults? string? string? string?))]
   [get-default-min-revision
    (or/c #f (-> package-query-defaults? string? string? string? string?))]
   [get-default-max-revision
    (or/c #f (-> package-query-defaults? string? string? string? string? string?))]
   [get-default-interval-bounds
    (or/c #f (-> package-query-defaults? (or/c "ii" "ie" "ei" "ee")))]))


(struct default-package-query-defaults-implementation ()
  #:methods gen:package-query-defaults
  [(define (get-default-provider c) DEFAULT_STRING)
   (define (get-default-package c p) DEFAULT_STRING)
   (define (get-default-edition c p n) DEFAULT_STRING)
   (define (get-default-min-revision c p n e) "0")
   (define (get-default-max-revision c p n e m) DEFAULT_STRING)
   (define (get-default-interval-bounds c) "ii")])

(define default-package-query-defaults
  (default-package-query-defaults-implementation))


(define (autocomplete-parsed-package-query cat query)
  (define (get accessor get-default)
    (let ([v (accessor query)])
      (if (non-empty-string? v)
          v
          (get-default))))

  (define provider
    (get parsed-package-query-provider-name
         (λ () (get-default-provider cat))))
  (define package
    (get parsed-package-query-package-name
         (λ () (get-default-package cat provider))))
  (define edition
    (get parsed-package-query-edition-name
         (λ () (get-default-edition cat provider package))))
  (define revision-min
    (get parsed-package-query-revision-min
         (λ () (get-default-min-revision cat provider package edition))))
  (define revision-max
    (get parsed-package-query-revision-max
         (λ () (get-default-max-revision cat provider package edition revision-min))))
  (define interval-bounds
    (get parsed-package-query-interval-bounds
         (λ () (get-default-interval-bounds cat))))

  (parsed-package-query
   provider
   package
   edition
   revision-min
   revision-max
   interval-bounds))


(define-generics package-query-canon
  [find-revision-number package-query-canon
                        provider package edition revision-name]
  [select-revision-number package-query-canon
                          provider package edition revision-min revision-max])

(define package-query-canon-implementation/c
  (package-query-canon/c [find-revision-number
                          (-> package-query-canon?
                              string?
                              string?
                              string?
                              string?
                              any/c)]
                         [select-revision-number
                          (-> package-query-canon?
                              string?
                              string?
                              string?
                              revision-number?
                              revision-number?
                              any/c)]))


(define (make-canonical-package-query
         #:force-complete-interval? [force-complete-interval? #f]
         canon
         defaults
         query-variant)
  (logged
   (λ (messages)
     (call/cc
      (λ (abort)
        ; Normalize argument
        (define query
          (if (package-query? query-variant)
              (parse-package-query query-variant)
              query-variant))
        (define autocompleted-but-unresolved
          (autocomplete-parsed-package-query defaults query))

        ; Check if we even need to bother. The output type is an exact
        ; package query.
        (when (exact-package-query? autocompleted-but-unresolved)
          (abort autocompleted-but-unresolved
                 messages))

        (define (find-revision-number* rev)
          (find-revision-number
           canon
           (parsed-package-query-provider-name autocompleted-but-unresolved)
           (parsed-package-query-package-name autocompleted-but-unresolved)
           (parsed-package-query-edition-name autocompleted-but-unresolved)
           rev))

        (define (select-revision-number* lo hi)
          (select-revision-number
           canon
           (parsed-package-query-provider-name autocompleted-but-unresolved)
           (parsed-package-query-package-name autocompleted-but-unresolved)
           (parsed-package-query-edition-name autocompleted-but-unresolved)
           lo
           hi))

        (define (make-message ctor . xs)
          (apply ctor
                 (format-parsed-package-query query)
                 (format-parsed-package-query autocompleted-but-unresolved)
                 xs))

        (define (fail . xs)
          (abort FAILURE
                 (append xs messages)))

        (define (handle-revision-number-interval lo hi)
          (define variant
            (if (equal? lo hi)
                hi
                (select-revision-number* lo hi)))
          (if (revision-number? variant)
              (if (or (< variant lo) (> variant hi))
                  (fail (make-message $package-query-canon:oob lo hi variant))
                  (abort (let ([final (~a variant)])
                           (struct-copy parsed-package-query autocompleted-but-unresolved
                                        [revision-min final]
                                        [revision-max final]
                                        [interval-bounds "ii"]))
                         messages))
              (fail (make-message $package-query-canon:no-selection lo hi variant))))

        (define lo (resolve-minimum-revision autocompleted-but-unresolved
                                             find-revision-number*))

        (define hi (resolve-maximum-revision autocompleted-but-unresolved
                                             find-revision-number*))


        (cond [(and (revision-number? hi) (revision-number? lo))
               (if (< hi lo)
                   (fail (make-message $package-query-canon:backwards lo hi))
                   (handle-revision-number-interval lo hi))]

              [(and (not (revision-number? hi))
                    (not (revision-number? lo)))
               (fail (make-message $package-query-canon:no-minimum lo)
                     (make-message $package-query-canon:no-maximum hi))]

              [(not (revision-number? hi))
               (if force-complete-interval?
                   (handle-revision-number-interval lo lo)
                   (fail (make-message $package-query-canon:no-maximum hi)))]

              [(not (revision-number? lo))
               (if force-complete-interval?
                   (handle-revision-number-interval hi hi)
                   (fail (make-message $package-query-canon:no-minimum lo)))]))))))



(module+ test
  (require racket/list
           rackunit)

  (test-equal? "Make exact query"
               (make-exact-package-query "acme" "anvil" "draft" 1)
               (parsed-package-query "acme" "anvil" "draft" "1" "1" "ii"))

  (test-case "Detect boundary flag strings"
    (check-true (boundary-flags-string? "ii"))
    (check-true (boundary-flags-string? "ie"))
    (check-true (boundary-flags-string? "ee"))
    (check-true (boundary-flags-string? "ei"))
    (check-false (boundary-flags-string? "e"))
    (check-false (boundary-flags-string? "i"))
    (check-false (boundary-flags-string? ""))
    (check-false (boundary-flags-string? "ab")))

  (test-equal? "Abbreviate queries"
               (abbreviate-exact-package-query (parsed-package-query "a" "b" "c" "0" "0" "ii"))
               "a:b:c:0")

  (test-case "Resolve minimum revisions"
    (define error-value (exn "broken" (current-continuation-marks)))
    (define (find-minimum-revision name)
      (case name
        [("ok") 8]
        [("break") error-value]
        [else "???"]))

    (define (check min-revision interval expected
                   [message (format "Resolve minimum revision: ~a ~a"
                                    min-revision
                                    interval)])
      (test-equal? message
                   (resolve-minimum-revision
                    (parsed-package-query "" "" "" min-revision "" interval)
                    find-minimum-revision)
                   expected))

    ; Trivially return given numbers
    (check 0 "ii" 0)
    (check 0 "ie" 0)
    (check 0 "ei" 1)
    (check 0 "ee" 1)
    (check "0" "ii" 0)
    (check "0" "ie" 0)
    (check "0" "ei" 1)
    (check "0" "ee" 1)

    ; Name resolution w/ boundary adjustments
    (check "ok" "ii" 8)
    (check "ok" "ie" 8)
    (check "ok" "ei" 9)
    (check "ok" "ee" 9)

    ; Raised error values
    (check "break" "ii" error-value)
    (check "break" "ie" error-value)
    (check "break" "ei" error-value)
    (check "break" "ee" error-value)

    ; Returned error values
    (check "?" "ii" "???")
    (check "?" "ie" "???")
    (check "?" "ei" "???")
    (check "?" "ee" "???"))


  (test-case "Resolve maximum revisions"
    (define error-value (exn "broken" (current-continuation-marks)))
    (define (find-maximum-revision name)
      (case name
        [("ok") 8]
        [("break") error-value]
        [else "???"]))

    (define (check max-revision interval expected
                   [message (format "Resolve maximum revision: ~a ~a"
                                    max-revision
                                    interval)])
      (test-equal? message
                   (resolve-maximum-revision
                    (parsed-package-query "" "" "" "" max-revision interval)
                    find-maximum-revision)
                   expected))

    ; Trivially return given numbers
    (check 5 "ii" 5)
    (check 5 "ie" 4)
    (check 5 "ei" 5)
    (check 5 "ee" 4)
    (check "5" "ii" 5)
    (check "5" "ie" 4)
    (check "5" "ei" 5)
    (check "5" "ee" 4)

    ; Account for zero
    (check 0 "ii" 0)
    (check 0 "ie" 0)
    (check 0 "ei" 0)
    (check 0 "ee" 0)
    (check "0" "ii" 0)
    (check "0" "ie" 0)
    (check "0" "ei" 0)
    (check "0" "ee" 0)

    ; Name resolution w/ boundary adjustments
    (check "ok" "ii" 8)
    (check "ok" "ie" 7)
    (check "ok" "ei" 8)
    (check "ok" "ee" 7)

    ; Raised error values
    (check "break" "ii" error-value)
    (check "break" "ie" error-value)
    (check "break" "ei" error-value)
    (check "break" "ee" error-value)

    ; Returned error values
    (check "?" "ii" "???")
    (check "?" "ie" "???")
    (check "?" "ei" "???")
    (check "?" "ee" "???"))



  (test-case "Convert between queries and strings"
    (define (verify s expected) (check-equal? (format-parsed-package-query (parse-package-query s)) expected))
    (verify ""                "")
    (verify "a"               "a")
    (verify "a:b"             "a:b")
    (verify "a:b:c"           "a:b:c")
    (verify "a:b:c:d"         "a:b:c:d")
    (verify "a:b:c:d:e"       "a:b:c:d:e")
    (verify "a:b:c:d:e:f"     "a:b:c:d:e:f")
    (verify "a:b:c:d:e:f:g"   "a:b:c:d:e:f")
    (verify "a:b:c::e:f:g:h"  "a:b:c::e:f"))


  (struct autocomplete-noop () #:methods gen:package-query-defaults [])

  (test-case "Autocomplete package queries"
    (test-not-exn "Do not throw a contract failure for missing autocompletion methods"
     (λ () (autocomplete-parsed-package-query
            (invariant-assertion package-query-defaults-implementation/c
                                 (autocomplete-noop))
            (parse-package-query ""))))

    (test-equal? "Build a default package query"
                 (autocomplete-parsed-package-query (autocomplete-noop)
                                                    (parse-package-query ""))
                 (parsed-package-query DEFAULT_STRING
                                       DEFAULT_STRING
                                       DEFAULT_STRING
                                       "0"
                                       DEFAULT_STRING
                                       "ii"))

    (test-equal? "Match default package query implementation with fallback implementation"
                 (autocomplete-parsed-package-query
                  default-package-query-defaults
                  (parse-package-query ""))
                 (autocomplete-parsed-package-query
                  (autocomplete-noop)
                  (parse-package-query "")))

    (struct autocomplete (pr pk ed)
      #:methods gen:package-query-defaults
      [(define (get-default-provider c)
         (autocomplete-pr c))
       (define (get-default-package c provider)
         (autocomplete-pk c))
       (define (get-default-edition c provider package)
         (autocomplete-ed c))
       (define (get-default-min-revision c provider package edition)
         (~a provider "-" package "-" edition "-initial"))
       (define (get-default-max-revision c provider package edition _)
         (~a provider "-" package "-" edition "-latest"))
       (define (get-default-interval-bounds c)
         "ei")])


    (test-equal? "Build a default package query a using custom catalog"
                 (autocomplete-parsed-package-query (autocomplete
                                                     "example.com"
                                                     "anvil"
                                                     "HEAVY")
                                                    (parse-package-query ""))
                 (parsed-package-query "example.com"
                                       "anvil"
                                       "HEAVY"
                                       "example.com-anvil-HEAVY-initial"
                                       "example.com-anvil-HEAVY-latest"
                                       "ei"))

    (test-equal? "Autocomplete every field that is not a non-empty string"
                 (autocomplete-parsed-package-query
                  (autocomplete "a" "b" "c")
                  (parsed-package-query #f
                                        "x"
                                        'fhkug
                                        ""
                                        ""
                                        "ii"))
                 (parsed-package-query "a"
                                       "x"
                                       "c"
                                       "a-x-c-initial"
                                       "a-x-c-latest"
                                       "ii")))

  (test-case "Make canonical package queries"
    (struct makes-canonical-queries ()
      #:methods gen:package-query-canon
      [(define (find-revision-number c pk pr e n)
         (case n
           [("initial") 0]
           [("latest") 9]
           [("missing") 'not-found]
           [else (raise "break!")]))
       (define (select-revision-number c pk pr e m n)
         9)])

    (struct no-canonical-queries ()
      #:methods gen:package-query-canon
      [])

    (define impl (makes-canonical-queries))

    (define (check-successful-canon l expected)
      (define-values (result messages)
        (run-log l null))
      (check-pred exact-package-query? result)
      (check-pred null? messages)
      (check-equal? result expected))

    (define (check-failing-canon l . preds)
      (define-values (result messages)
        (run-log l null))
      (check-eq? result FAILURE)
      (check-equal? (length messages) (length preds))
      (map (λ (pred msg) (pred msg))
           preds
           (flatten messages)))

    (define (get-failing-canon-log l)
      (flatten (get-log l)))

    (define (get-failing-canon-message l)
      (let ([log (get-log l)])
        (and (not (null? log))
             (car log))))

    (test-case "Use exn:fail:contract for catalog-implements-canonical-queries/c"
      (check-failing-canon
       (make-canonical-package-query #:force-complete-interval? #f
                                     (invariant-assertion
                                      package-query-canon-implementation/c
                                      (no-canonical-queries))
                                     (autocomplete-noop)
                                     (parse-package-query ":::X:Y"))
       (λ (v)
         (check-pred $package-query-canon:no-minimum? v)
         (check-pred exn:fail:contract? ($package-query-canon:no-minimum-hint v)))
       (λ (v)
         (check-pred $package-query-canon:no-maximum? v)
         (check-pred exn:fail:contract? ($package-query-canon:no-maximum-hint v)))))

    (test-case "Create a canonical query with existing revision names"
      (define expected
        (parsed-package-query DEFAULT_STRING
                              "pkg"
                              DEFAULT_STRING
                              "9"
                              "9"
                              "ii"))

      (define as-query  ":pkg::initial:latest")
      (define as-parsed (parse-package-query as-query))
      (define (check v)
        (check-successful-canon
         (make-canonical-package-query #:force-complete-interval? #f
                                       impl
                                       (autocomplete-noop)
                                       v)
         expected))
      (check as-query)
      (check as-parsed)
      (check ":pkg::0:9"))

    (test-case "Do not create canonical query with missing minimum revision when force-complete-interval? is #f"
      (check-equal?
       (get-failing-canon-message
        (make-canonical-package-query #:force-complete-interval? #f
                                      impl
                                      (autocomplete-noop)
                                      (parse-package-query ":pkg::initial:missing")))
       ($package-query-canon:no-maximum ":pkg::initial:missing"
                                        (string-join (list DEFAULT_STRING
                                                           "pkg"
                                                           DEFAULT_STRING
                                                           "initial"
                                                           "missing"
                                                           "ii")
                                                     ":")
                                        'not-found)))

    (test-case "Do not create canonical query with missing minimum revision when force-complete-interval? is #f"
      (check-equal?
       (get-failing-canon-message
        (make-canonical-package-query #:force-complete-interval? #f
                                      impl
                                      (autocomplete-noop)
                                      (parse-package-query ":pkg::missing:latest")))
       ($package-query-canon:no-minimum ":pkg::missing:latest"
                                        (string-join (list DEFAULT_STRING
                                                           "pkg"
                                                           DEFAULT_STRING
                                                           "missing"
                                                           "latest"
                                                           "ii")
                                                     ":")
                                        'not-found)))

    (test-case "Create canonical query with new minimum revision when force-complete-interval? is a true value"
      (check-successful-canon
       (make-canonical-package-query #:force-complete-interval? 'whatever
                                     impl
                                     (autocomplete-noop)
                                     (parse-package-query ":pkg::missing:latest"))
       (parsed-package-query DEFAULT_STRING
                             "pkg"
                             DEFAULT_STRING
                             "9"
                             "9"
                             "ii")))

    (test-case "Create canonical query with new maximum revision when force-complete-interval? is a true value"
      (check-successful-canon
       (make-canonical-package-query #:force-complete-interval? #t
                                     impl
                                     (autocomplete-noop)
                                     (parse-package-query ":pkg::initial:missing"))
       (parsed-package-query DEFAULT_STRING
                             "pkg"
                             DEFAULT_STRING
                             "0"
                             "0"
                             "ii")))

    (test-case "Never create canonical query missing both revisions"
      (define q ":pkg::X:missing")
      (define pq (parse-package-query q))
      (define expected-ac (string-join (list DEFAULT_STRING "pkg" DEFAULT_STRING "X" "missing" "ii") ":"))
      (define expected
        (list ($package-query-canon:no-maximum q expected-ac 'not-found)
              ($package-query-canon:no-minimum q expected-ac "break!")))
      (check-equal?
       (get-failing-canon-log
        (make-canonical-package-query #:force-complete-interval? #f
                                      impl
                                      (autocomplete-noop)
                                      pq))
       expected)
      (check-equal?
       (get-failing-canon-log
        (make-canonical-package-query #:force-complete-interval? #t
                                      impl
                                      (autocomplete-noop)
                                      pq))
       expected))

    (test-case "Never create canonical queries with backwards intervals"
      (define q ":pkg::latest:initial")
      (define pq (parse-package-query q))
      (define expected-ac (string-join (list DEFAULT_STRING "pkg" DEFAULT_STRING "latest" "initial" "ii") ":"))
      (define expected ($package-query-canon:backwards q expected-ac 9 0))
      (check-equal?
       (get-failing-canon-message
        (make-canonical-package-query #:force-complete-interval? #f
                                      impl
                                      (autocomplete-noop)
                                      pq))
        expected)
      (check-equal?
       (get-failing-canon-message
        (make-canonical-package-query #:force-complete-interval? #t
                                      impl
                                      (autocomplete-noop)
                                      pq))
       expected))

    (test-case "Never create canonical queries based on invalid selections"
      (struct selects-too-large ()
        #:methods gen:package-query-canon
        [(define (find-revision-number c pk pr e n) #f)
         (define (select-revision-number c pk pr e m n) (add1 n))])
      (struct selects-too-small ()
        #:methods gen:package-query-canon
        [(define (find-revision-number c pk pr e n) #f)
         (define (select-revision-number c pk pr e m n) (sub1 m))])

      (check-match (get-failing-canon-message
                    (make-canonical-package-query (selects-too-large)
                                                  (autocomplete-noop)
                                                  ":::0:10:ii"))
                   ($package-query-canon:oob _ _ 0 10 11))
      (check-match (get-failing-canon-message
                    (make-canonical-package-query (selects-too-small)
                                                  (autocomplete-noop)
                                                  ":::1:10:ii"))
                   ($package-query-canon:oob _ _ 1 10 0))
      (check-match (get-failing-canon-message
                    (make-canonical-package-query (selects-too-small)
                                                  (autocomplete-noop)
                                                  ":::0:10:ii"))
                   ($package-query-canon:no-selection _ _ 0 10 -1)))))
