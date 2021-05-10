#lang racket/base

(require racket/contract)

(define shovel/c
  (-> any/c (subprogram/c artifact-info?)))

(provide
 (contract-out
  [find-artifact
   (->* (any/c)
        (shovel/c)
        (subprogram/c artifact-info?))]
  [shovel/c
   chaperone-contract?]
  [broken-shovel
   shovel/c]
  [dig-failure
   (-> (or/c symbol? string?)
       any/c
       subprogram?)]
  [current-shovel
   (parameter/c shovel/c)]
  [shovel-cons
   (-> shovel/c shovel/c shovel/c)]
  [shovel-list
   (->* () #:rest (listof shovel/c) shovel/c)]))


(require "artifact.rkt"
         "subprogram.rkt"
         "message.rkt"
         "query.rkt")


(define+provide-message $dig ())
(define+provide-message $dig:no-artifact $dig (shovel-name hint))


(define (dig-failure name hint)
  (subprogram-failure ($dig:no-artifact name hint)))

(define (broken-shovel v)
  (dig-failure (object-name broken-shovel) v))


(define ((shovel-cons a b) k)
  (subprogram (λ (m)
                (define-values (r m*) (run-subprogram (a k) m))
                (if (artifact-info? r)
                    (values r m*)
                    (run-subprogram (b k) m*)))))


(define (shovel-list . xs)
  (if (null? xs)
      broken-shovel
      (shovel-cons
       (car xs)
       (apply shovel-list
              (cdr xs)))))


(define current-shovel
  (make-parameter broken-shovel))


(define-subprogram (find-artifact plinth [dig (current-shovel)])
  (if (artifact-info? plinth)
      ($use plinth)
      ($run! (dig plinth))))


(module+ test
  (require rackunit
           (submod "subprogram.rkt" test))

  (test-case "Combine shovels"
    (define ((bind-shovel v) k)
      (if (eq? k v)
          (subprogram-unit (artifact v))
          (dig-failure v k)))

    (define shovel
      (shovel-list
       (bind-shovel #"a")
       (bind-shovel #"b")
       (bind-shovel #"c")))

    (define (check expected)
      (check-equal? (artifact-info-source (get-subprogram-value (shovel expected)))
                    expected))

    (check #"a")
    (check #"b")
    (check #"c")

    (define-values (should-be-failure messages)
      (run-subprogram (shovel #"d")))

    (check-eq? should-be-failure FAILURE)
    (check-equal? messages
                  (list ($dig:no-artifact 'broken-shovel #"d")
                        ($dig:no-artifact #"c" #"d")
                        ($dig:no-artifact #"b" #"d")
                        ($dig:no-artifact #"a" #"d"))))

  (test-subprogram-value
   "Trivially find provided artifact"
   (find-artifact (artifact #"") broken-shovel)
   (λ (result)
     (check-equal? (artifact-info-source result) #"")))

  (test-subprogram
   "Do not find an artifact by default when none is available"
   (find-artifact #f)
   (λ (result messages)
     (check-equal? result FAILURE)
     (check-equal? messages
                   (list ($dig:no-artifact 'broken-shovel #f)))))

  (test-case "Find an artifact using the right shovel"
    (define (indy req)
      (subprogram-unit
       (artifact
        (if req #"t" #"f"))))

    (define (check v expected)
      (check-subprogram-value
       (find-artifact v indy)
       (λ (result)
         (check-equal? (artifact-info-source result) expected))))

    (check #t #"t")
    (check #f #"f")))
