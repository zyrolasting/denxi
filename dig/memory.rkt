#lang racket/base

(require racket/contract)
(provide
 (contract-out
  [make-memory-shovel
   (-> (hash/c any/c artifact-info?) shovel/c)]
  [make-memory-shovel/pkgdef
   (->* (hash?)
        (package-query-defaults-implementation/c)
        shovel/c)]))

(require racket/function
         racket/format
         racket/match
         "../artifact.rkt"
         "../dig.rkt"
         "../input.rkt"
         "../integrity.rkt"
         "../subprogram.rkt"
         "../monad.rkt"
         "../query.rkt"
         "../signature.rkt"
         "../version.rkt")


(define ((make-memory-shovel contents) key)
  (if (hash-has-key? contents key)
      (subprogram-unit (hash-ref contents key))
      (dig-failure 'make-memory-shovel key)))


(define (make-memory-shovel/pkgdef contents [defaults default-package-query-defaults])
  (let ([canon (memory-canon contents)])
    (λ (key)
      (call/cc
       (λ (abort)
         (define (fail v)
           (abort (dig-failure 'make-memory-shovel/pkgdef key)))
         (if (package-query-variant? key)
             (mdo exact-query := (make-canonical-package-query canon defaults key)
                  (subprogram-unit (call-with-revisions
                                    contents
                                    exact-query
                                    (λ (revisions n)
                                      (ref revisions n values)))))
             (fail key)))))))


(struct memory-canon (contents)
  #:methods gen:package-query-canon
  [(define (find-revision-number cat P K E A)
     (call-with-revisions
      (memory-canon-contents cat)
      (parsed-package-query P K E A A "ii")
      (λ (revisions _)
        (hash-ref revisions A #f))))

   (define (select-revision-number cat P K E L H)
     (call-with-revisions
      (memory-canon-contents cat)
      (parsed-package-query P K E (~a L) (~a H) "ii")
      (λ (revisions _)
        (find-latest-available-revision-number
         (λ (v) (hash-ref revisions v #f))
         L H))))])


(define (call-with-revisions contents exact-query continue-with)
  (match-let ([(parsed-package-query provider package edition rev _ _) exact-query])
    (call/cc
     (λ (abort)
       (define packages (ref contents provider abort))
       (define editions (ref packages package abort))
       (define revisions (ref editions edition abort))
       (define revision-number (string->number rev))
       (continue-with revisions revision-number)))))


(define (ref h k abort)
  (define v (hash-ref h k #f))
  (cond [(not v) (abort v)]
        [(string? v) (ref h v abort)]
        [else v]))


(module+ test
  (require rackunit
           "../subprogram.rkt"
           "../source.rkt"
           (submod "../subprogram.rkt" test))

  (test-equal? "Alias keys"
               (ref (hash "a" 1
                          "b" "a"
                          "c" "b")
                    "c"
                    void)
               1)

  (test-pred "Abort on #f"
             void?
             (ref (hash "a" #f) "a" void))

  (test-case "Search memory-catalog"
    (define expected
      (artifact (byte-source #"def") #f #f))

    (define (check-expected q)
      (define-values (actual m) (run-subprogram (dig q)))
      (check-pred artifact-info? actual)
      (match-define (artifact-info (byte-source edata) eii esi) expected)
      (match-define (artifact-info (byte-source adata) aii asi) actual)
      (check-equal? adata edata)
      (check-equal? aii eii)
      (check-equal? asi esi))

    (define contents
      (hash "default"
            (hash
             "default"
             (hash "default"
                   (hash 0 expected
                         "default" 0)))
            "jon"
            (hash "calculator"
                  (hash "default"
                        (hash 9 expected
                              "beta" 9
                              "cool" 9)))))

    (define dig (make-memory-shovel/pkgdef contents))

    (check-expected "")
    (check-expected "jon:calculator::beta:beta")
    (check-expected "jon:calculator::cool:cool")
    (check-expected "jon:calculator::beta:cool")))
