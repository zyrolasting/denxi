#lang racket/base

; Define integrity check operation

(require racket/contract)

(provide (struct-out $integrity)
         (struct-out integrity-info)
         (contract-out
          [check-integrity
           (-> #:trust-bad-digest any/c
               (-> symbol? any/c)
               any/c
               bytes?
               $integrity?)]
          [well-formed-integrity-info/c
           flat-contract?]))

(require (only-in "../message.rkt" define-message))

(define-message $integrity (ok? stage info))

(struct integrity-info (algorithm digest) #:prefab)

(define well-formed-integrity-info/c
  (struct/c integrity-info symbol? bytes?))


(define (check-integrity #:trust-bad-digest trust-bad-digest
                         trust-chf?
                         intinfo
                         other-digest)
  (if trust-bad-digest
      ($integrity #t 'consider-digest-trust intinfo)
      (if (well-formed-integrity-info/c intinfo)
          (if (trust-chf? (integrity-info-algorithm intinfo))
              ($integrity (equal? (integrity-info-digest intinfo) other-digest)
                          'consider-digest-match
                          intinfo)
              ($integrity #f 'consider-chf-trust intinfo))
          ($integrity trust-bad-digest 'consider-integrity-info intinfo))))


(module+ test
  (require rackunit)

  (define info (integrity-info 'whatever #""))

  (test-equal? "Trust bad digests"
               (check-integrity #:trust-bad-digest #t #f info #f)
               ($integrity #t 'consider-digest-trust info))

  (test-equal? "Distrust CHFs"
               (check-integrity #:trust-bad-digest #f (λ _ #f) info #f)
               ($integrity #f 'consider-chf-trust info))

  (test-equal? "Reject bad integrity info"
               (check-integrity #:trust-bad-digest #f (λ _ #t) 'junk #f)
               ($integrity #f 'consider-integrity-info 'junk))

  (test-equal? "Trust CHFs"
               (check-integrity #:trust-bad-digest #f (λ _ #t) info #"wont match")
               ($integrity #f 'consider-digest-match info))

  (test-equal? "Match digests"
               (check-integrity #:trust-bad-digest #f (λ _ #t) info #"")
               ($integrity #t 'consider-digest-match info)))
