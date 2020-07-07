#lang racket/base

(require zcpkg/dependency
         zcpkg/zcpkg-info
         zcpkg/revision
         zcpkg/url)


(module+ test
  (require rackunit)

  (test-pred "Detect well-formed dependencies"
             well-formed-dependency?
             (dependency "provider" "package" "edition" #t "0" #t "0"))

  (define ill-formed
    (list (dependency "" "package" "edition" #t "0" #t "0")
          (dependency "provider" "" "edition" #t "0" #t "0")
          (dependency "provider" "edition" "" #t "0" #t "0")
          (dependency "provider" "package" "edition" "#t" "0" #t "0")
          (dependency "provider" "package" "edition" #t #f #t "0")
          (dependency "provider" "package" "edition" #t "0" "#t" "0")
          (dependency "provider" "package" "edition" #t "0" #t #f)
          (dependency #f #f #f #f #f #f #f)))

  (for ([example (in-list ill-formed)])
    (test-false (format "Detect ill-formed dependency: ~s" example)
                (well-formed-dependency? example)))

  (test-true "Detect concrete dependency"
             (concrete-dependency? (dependency "provider" "package" "edition" #t "0" #t "10")))
  (test-false "Detect abstract dependency"
              (concrete-dependency? (dependency "provider" "package" "edition" #t "initial" #t "10")))

  (test-true "Exact dependency: One possible version"
             (exact-dependency? (dependency "p" "p" "p" #f "0" #f "0")))
  (test-false "Prohibit exact dependency from having exclusive lower bound"
              (exact-dependency? (dependency "p" "p" "p" #t "0" #f "0")))
  (test-false "Prohibit exact dependency from having exclusive upper bound"
              (exact-dependency? (dependency "p" "p" "p" #f "0" #t "0")))
  (test-false "Prohibit exact dependency from varying on version"
              (exact-dependency? (dependency "p" "p" "p" #f "0" #f "1")))

  (test-case "Convert between dependency instances and their representations"
    (define target (dependency "joe" "pkg" "edition" #f "8" #f "8"))
    (define str-repr "joe/pkg;edition;i;8;i;8")
    (define url-repr (string->url str-repr))
    (define info-repr (zcpkg-info "joe" "pkg" "edition" "8" #f #f #f #f #f #f))
    (check-equal? target (string->dependency str-repr))
    (check-equal? target (coerce-dependency str-repr))
    (check-equal? target (url->dependency url-repr))
    (check-equal? target (coerce-dependency url-repr))
    (check-equal? target (zcpkg-info->dependency info-repr))
    (check-equal? target (coerce-dependency info-repr))
    (check-equal? str-repr (dependency->string target))
    (check-equal? (url-path url-repr) (url-path (dependency->url target))))


  (test-true "Detect equal dependency identities"
             (dependency-identity=? (dependency "a" "b" "c" #f #f #f #f)
                                    (dependency "a" "b" "c" #f #f #f #f)))

  (test-false "Detect differing provider names"
              (dependency-identity=? (dependency "a" "b" "c" #f #f #f #f)
                                     (dependency " " "b" "c" #f #f #f #f)))

  (test-false "Detect differing package names"
              (dependency-identity=? (dependency "a" "b" "c" #f #f #f #f)
                                     (dependency "a" " " "c" #f #f #f #f)))

  (test-false "Detect differing edition names"
              (dependency-identity=? (dependency "a" "b" "c" #f #f #f #f)
                                     (dependency "a" "b" " " #f #f #f #f)))


  (test-case "Use Strings and URLs to produce exact and inexact dependencies"
    (define (check-conversion dep str)
      (check-equal? dep (string->dependency str))
      (check-equal? dep (url->dependency (string->url str))))

    (check-conversion (dependency "joe" "pkg" "draft" #f "newest" #f "newest")
                      "joe/pkg")

    (check-conversion (dependency "joe" "pkg" "edition" #f "newest" #f "newest")
                      "joe/pkg;edition")

    (check-conversion (dependency "joe" "pkg" "edition" #f "initial" #f "initial")
                      "joe/pkg;edition;initial")

    (check-conversion (dependency "joe" "pkg" "edition" #f "min" #f "max")
                      "joe/pkg;edition;min;max")

    (check-conversion (dependency "joe" "pkg" "edition" #f "beta" #t "prod")
                      "joe/pkg;edition;i;beta;e;prod"))

  (test-pred "Use zcpkg-info instances to produce exact dependencies"
             exact-dependency?
             (zcpkg-info->dependency (zcpkg-info "joe" "pkg" "edition" "8" #f #f #f #f #f #f)))


  (test-true "Match a dependency using a revision range"
             (dependency-match? "joe/pkg;draft;0;100" "joe/pkg;draft;0;0"))

  (test-true "Match a dependency using a revision range, even with string+number mixes"
             (dependency-match? (dependency "joe" "pkg" "draft" #f "0" #f 100)
                                (dependency "joe" "pkg" "draft" #f 0 #f "0")))

  (test-true "Match a dependency exactly"
             (dependency-match? "joe/pkg;draft;2;2" "joe/pkg;draft;2;2"))

  (test-true "Match a dependency that risks an off-by-one error (lower bound)"
             (dependency-match? "joe/pkg;draft;e;1;i;3"
                                "joe/pkg;draft;2;2"))

  (test-true "Match a dependency that risks an off-by-one error (upper bound)"
             (dependency-match? "joe/pkg;draft;i;1;e;3"
                                "joe/pkg;draft;2;2"))

  (test-false "Do not match a dependency that differs in provider name"
              (dependency-match? "joe/pkg;draft;i;1;e;3"
                                 "je/pkg;draft;2;2"))

  (test-false "Do not match a dependency that differs in package name"
              (dependency-match? "joe/pkg;draft;i;1;e;3"
                                 "joe/pg;draft;2;2"))

  (test-false "Do not match a dependency that differs in edition name"
              (dependency-match? "joe/pkg;draft;i;1;e;3"
                                 "joe/pkg;drft;2;2"))

  (test-exn "Raise a contract error if comparing two inexact dependencies"
            #rx"expected: An exact dependency"
            (λ () (dependency-match? "joe/pkg;draft;i;1;e;3"
                                     "joe/pkg")))

  (test-exn "Raise a contract error if matching against an ill-formed dependency"
            #rx"expected: A concrete dependency"
            (λ () (dependency-match? (dependency #f #f #f #f #f #f #f)
                                     "joe/pkg;draft;1;1")))

  (test-exn "Raise a contract error if matching against an abstract dependency"
            #rx"expected: A concrete dependency"
            (λ () (dependency-match? "joe/pkg"
                                     "joe/pkg;draft;1;1")))

  (test-exn "Raise a special error if matching against an invalid interval"
            exn:fail:zcpkg:invalid-revision-interval?
            (λ () (dependency-match? "joe/pkg;draft;10;1"
                                     "joe/pkg;draft;1"))))
