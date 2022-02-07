#lang racket/base

(require racket/contract)

(provide (struct-out artifact)
         (struct-out $artifact)
         (struct-out $artifact:signature)
         (struct-out $artifact:integrity)
         (struct-out $dig)
         (struct-out $dig:no-artifact)
         (contract-out
          [verify-artifact
           (-> artifact?
               known-implementation/c
               (machine/c void?))]
          [make-artifact
           (->* (source-variant?)
                ((or/c #f integrity?)
                 (or/c #f signature?))
                artifact?)]
          [study-artifact
           (-> artifact?
               transfer-policy?
               (machine/c input-port?))]
          [find-artifact
           (->* (any/c)
                (shovel/c)
                (machine/c artifact?))]
          [shovel/c
           chaperone-contract?]
          [broken-shovel
           shovel/c]
          [dig-failure
           (-> (or/c symbol? string?)
               any/c
               machine?)]
          [shovel-cons
           (-> shovel/c shovel/c shovel/c)]
          [shovel-list
           (->* () #:rest (listof shovel/c) shovel/c)]))


(require racket/match
         "format.rkt"
         "integrity.rkt"
         "machine.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt"
         "signature.rkt"
         "source.rkt")


(define-message $artifact ())
(define-message $artifact:integrity (status chf-name))
(define-message $artifact:signature (status public-key))


(define shovel/c
  (-> any/c (machine/c artifact?)))

(require "artifact.rkt"
         "monad.rkt"
         "machine.rkt"
         "message.rkt")


(define-message $dig ())
(define-message $dig:no-artifact $dig (shovel-name hint))


(define (dig-failure name hint)
  (machine-failure ($dig:no-artifact name hint)))


(define (broken-shovel v)
  (dig-failure (object-name broken-shovel) v))


(define ((shovel-cons a b) k)
  (machine (λ (m)
                (define-values (r m*) (run-machine (a k) m))
                (if (artifact? r)
                    (values r m*)
                    (run-machine (b k) m*)))))


(define (shovel-list . xs)
  (if (null? xs)
      broken-shovel
      (shovel-cons
       (car xs)
       (apply shovel-list
              (cdr xs)))))


(define-machine (find-artifact plinth [dig (current-shovel)])
  (if (artifact? plinth)
      ($use plinth)
      ($run! (dig plinth))))


(module+ test
  (require rackunit
           racket/file
           (submod "machine.rkt" test))

  (test-case "Combine shovels"
    (define ((bind-shovel v) k)
      (if (eq? k v)
          (machine-unit (make-artifact v))
          (dig-failure v k)))

    (define shovel
      (shovel-list
       (bind-shovel #"a")
       (bind-shovel #"b")
       (bind-shovel #"c")))

    (define (check expected)
      (check-equal? (artifact-source (get-machine-value (shovel expected)))
                    expected))

    (check #"a")
    (check #"b")
    (check #"c")

    (define-values (should-be-failure messages)
      (run-machine (shovel #"d")))

    (check-eq? should-be-failure FAILURE)
    (check-equal? messages
                  (list ($dig:no-artifact 'broken-shovel #"d")
                        ($dig:no-artifact #"c" #"d")
                        ($dig:no-artifact #"b" #"d")
                        ($dig:no-artifact #"a" #"d"))))

  (test-machine-value
   "Trivially find provided artifact"
   (find-artifact (make-artifact #"") broken-shovel)
   (λ (result)
     (check-equal? (artifact-source result) #"")))

  (test-machine
   "Do not find an artifact by default when none is available"
   (find-artifact #f)
   (λ (result messages)
     (check-equal? result FAILURE)
     (check-equal? messages
                   (list ($dig:no-artifact 'broken-shovel #f)))))

  (test-case "Find an artifact using the right shovel"
    (define (indy req)
      (machine-unit
       (make-artifact
        (if req #"t" #"f"))))

    (define (check v expected)
      (check-machine-value
       (find-artifact v indy)
       (λ (result)
         (check-equal? (artifact-source result) expected))))

    (check #t #"t")
    (check #f #"f")))
