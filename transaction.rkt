#lang racket/base

; Consider the following command line, where -i and -u can be
; specified multiple times. Each value is accumulated into
; a list for the corresponding flag.
;
;  $ xiden pkg -i a -u b -i c -u d
;
; -i produces (c a) when accumulated
; -u produces (d b) when accumulated
;
; The user expects actions to run in the visible order: (a b c d).
; This module transforms the accumulated lists into procedures that
; run in the expected order.


(require "contract.rkt")

(provide
 (contract-out
  [transact
   (-> (listof (-> logged?))
       (-> list? any)
       (-> list? any)
       any)]
  [fold-transaction-actions
   (-> (listof (cons/c procedure? any/c))
       (hash/c procedure? (-> any/c logged?))
       (listof (-> logged?)))]))


(require "exn.rkt"
         "message.rkt"
         "package.rkt")


(define (transact actions commit rollback)
  (define (update action remaining-actions accum-messages)
    (define-values (result messages)
      (with-handlers
        ([values (λ (e) (raise (cons ($show-string (exn->string e))
                                     accum-messages)))])
        (run-log (action))))

    (if (eq? result FAILURE)
        (rollback (cons messages accum-messages))
        (enter remaining-actions (cons messages accum-messages))))

  (define (enter remaining-actions accum-messages)
    (if (null? remaining-actions)
        (commit accum-messages)
        (update (car remaining-actions)
                (cdr remaining-actions)
                accum-messages)))

  (with-handlers ([values rollback])
    (enter actions null)))


(define (fold-transaction-actions flags lookup)
  (fold-transaction-actions-aux flags lookup null (hasheq)))


(define (fold-transaction-actions-aux flags lookup actions counts)
  (if (null? flags)
      (reverse actions)
      (let ([setting (caar flags)])
        (if (hash-has-key? lookup setting)
            (add-action flags
                        lookup
                        actions
                        setting
                        (hash-set counts setting (add1 (hash-ref counts setting -1))))
            (fold-transaction-actions-aux (cdr flags)
                                          lookup
                                          actions
                                          counts)))))

(define (make-action lookup setting value)
  (λ () ((hash-ref lookup setting) value)))


(define (add-action flags lookup actions setting counts)
  (fold-transaction-actions-aux (cdr flags)
                                lookup
                                (cons (make-action lookup
                                                   setting
                                                   (list-ref (setting)
                                                             (hash-ref counts setting)))
                                      actions)
                                counts))


(module+ test
  (require rackunit
           racket/list
           racket/match)

  (define (mocker f) (λ (i) (cons f (list-ref (f) i))))

  (test-case "Bind multi flags in thunks"
    (define (letters) '(a b c))
    (define (numbers) '(1 2 3))
    (define (herring) '(red))
    (define mock-letter  (mocker letters))
    (define mock-number  (mocker numbers))
    (define mock-herring (mocker herring))

    (define actions
      (fold-transaction-actions (list (mock-letter  0)
                                      (mock-number  0)
                                      (mock-herring 0)
                                      (mock-herring 0)
                                      (mock-number  1)
                                      (mock-herring 0)
                                      (mock-letter  1)
                                      (mock-herring 0)
                                      (mock-letter  2)
                                      (mock-number  2))
                                (hasheq letters symbol->string
                                        numbers -)))

    (check-equal? (map (λ (f) (f)) actions)
                  '("a" -1 -2 "b" "c" -3)))

  (test-case "Carry out successful transaction"
    (define (alice) '("hi"  "how are you" "great" "bye"))
    (define (bob)   '("hey" "im doing okay" "got a new kid" "ikr" "see ya"))
    (define (mocker f) (λ (i) (cons f (list-ref (f) i))))
    (define (read-convo v)
      (logged-attachment (if (equal? v "see ya")
                             SUCCESS
                             #f)
                         ($show-string v)))

    (define mock-alice (mocker alice))
    (define mock-bob   (mocker bob))

    (define flags
      (list (mock-alice 0)
            (mock-bob 0)
            (mock-alice 1)
            (mock-bob 1)
            (mock-bob 2)
            (mock-alice 2)
            (mock-bob 3)
            (mock-alice 3)
            (mock-bob 4)))

    (define actions
      (fold-transaction-actions flags
                                (hasheq alice read-convo
                                        bob read-convo)))

    (define (rollback m)
      (cons 'rolled-back (flatten m)))

    (check-equal? (transact actions flatten rollback)
                  (map (λ (pair) ($show-string (cdr pair)))
                       (reverse flags)))

    (define (warn) (logged-attachment #f ($show-string "about to fail")))
    (test-equal? "Handle transaction failure"
                 (transact (list warn
                                 (λ () (logged-failure ($show-string "uh oh")))
                                 warn)
                           flatten
                           rollback)
                 (list 'rolled-back
                       ($show-string "uh oh")
                       ($show-string "about to fail")))

    (test-equal? "Handle transaction failure via raised value"
                 (transact (list warn (λ () (raise 'oops)) warn)
                           flatten
                           rollback)
                 (list 'rolled-back
                       ($show-string "oops\n")
                       ($show-string "about to fail")))))
