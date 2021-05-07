#lang racket/base

(require racket/contract)

(define shovel/c
  (-> any/c (logged/c artifact-info?)))

(provide
 (contract-out
  [find-artifact
   (->* (any/c)
        (shovel/c)
        (logged/c artifact-info?))]
  [shovel/c
   chaperone-contract?]
  [broken-shovel
   shovel/c]
  [current-shovel
   (parameter/c shovel/c)]))


(require "artifact.rkt"
         "logged.rkt"
         "message.rkt"
         "query.rkt")


(define+provide-message $dig ())
(define+provide-message $dig:no-artifact $dig (hint))


(define (broken-shovel v)
  (logged-failure ($dig:no-artifact v)))


(define ((shovel-cons a b) k)
  (logged (λ (m)
            (define-values (r m*) (run-log (a k) m))
            (if (artifact-info? r)
                (values r m*)
                (run-log (b k) m*)))))


(define (shovel-list . xs)
  (if (null? xs)
      broken-shovel
      (shovel-cons
       (car xs)
       (apply shovel-list
              (cdr xs)))))


(define current-shovel
  (make-parameter broken-shovel))


(define-logged (find-artifact plinth [dig (current-shovel)])
  (if (artifact-info? plinth)
      ($use plinth)
      ($run! (dig plinth))))


(module+ test
  (require rackunit
           (submod "logged.rkt" test))

  (test-logged-value
   "Trivially find provided artifact"
   (find-artifact (artifact #"") broken-shovel)
   (λ (result)
     (check-equal? (artifact-info-source result) #"")))

  (test-logged-procedure
   "Do not find an artifact by default when none is available"
   (find-artifact #f)
   (λ (result messages)
     (check-equal? result FAILURE)
     (check-equal? messages
                   (list ($dig:no-artifact #f)))))

  (test-case "Find an artifact using the right shovel"
    (define (indy req)
      (logged-unit
       (artifact
        (if req #"t" #"f"))))

    (define (check v expected)
      (check-logged-value
       (find-artifact v indy)
       (λ (result)
         (check-equal? (artifact-info-source result) expected))))

    (check #t #"t")
    (check #f #"f")))
