#lang racket/base

(require racket/contract)
(provide (contract-out
          [canon/c contract?]
          [default-canon canon/c]
          [current-canon (parameter/c canon/c)]
          [canonicalize (->* (any/c any/c) (canon/c) any/c)]))


(define (canonicalize domain value [canon (current-canon)])
  (canon domain value canon))


(define canon/c
  (recursive-contract
   (-> any/c any/c canon/c any/c)))


(define (default-canon domain value canon)
  value)


(define current-canon
  (make-parameter default-canon))


(module+ test
  (require racket/string rackunit)

  (test-case "Canonicalize values"
    (define (xbox-release-number domain element canon)
      (case domain
        [(console)
         (case (regexp-replace* #px"xbox\\s+" (string-downcase element) "")
           [("") 1]
           [("360") 2]
           [("360 s") 3]
           [("360 e") 4]
           [("one") 5]
           [("one s") 6]
           [("one x") 7]
           [("series x") 8]
           [else #f])]
        [(gossip)
         (canon 'console
                (if (string-contains? element "latest")
                    "series x"
                    element)
                canon)]
        [else #f]))

    (parameterize ([current-canon xbox-release-number])
      (check-false (canonicalize 'console "720"))
      (check-equal? (canonicalize 'console "Xbox 360") 2)
      (check-equal? (canonicalize 'gossip "the latest one") 8))))
