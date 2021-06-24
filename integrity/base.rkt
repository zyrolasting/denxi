#lang racket/base

; Define integrity check operation

(require racket/contract)

(provide (struct-out $integrity)
         (struct-out integrity-info)
         (contract-out
          [check-integrity
           (-> #:trust-bad-digest any/c
               #:trust-message-digest-algorithms (listof symbol?)
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

(define (make-$integrity ok? p intinfo)
  ($integrity ok? (object-name p) intinfo))


(define (check-integrity #:trust-bad-digest trust-bad-digest
                         #:trust-message-digest-algorithms trust-message-digest-algorithms
                         intinfo
                         variant)
  (consider-digest-trust #:trust-bad-digest trust-bad-digest intinfo
   (λ () (consider-integrity-info #:trust-unknown-digest trust-bad-digest intinfo
     (λ () (consider-chf-trust #:trust-message-digest-algorithms trust-message-digest-algorithms intinfo
       (λ () (consider-digest-match intinfo variant))))))))



; Danger! Trusting a bad digest means trusting arbitrary data
(define (consider-digest-trust #:trust-bad-digest trust-bad-digest intinfo k)
  (if trust-bad-digest
      (make-$integrity #t consider-digest-trust intinfo)
      (k)))


; CHFs may have collisions, and we cannot predict InfoSec incidents.
; Proceed only if the caller says they trust the CHF.
(define (consider-chf-trust #:trust-message-digest-algorithms trusted intinfo k)
  (if (member (integrity-info-algorithm intinfo) trusted)
      (k)
      (make-$integrity #f consider-chf-trust intinfo)))


; We cannot reasonably compare digests if the integrity information is
; malformed. Make sure we can use it.
(define (consider-integrity-info #:trust-unknown-digest trust-unknown-digest intinfo k)
  (if (well-formed-integrity-info/c intinfo)
      (k)
      (make-$integrity trust-unknown-digest
                       consider-integrity-info
                       intinfo)))


; Compare the digests as the last step to protect preconditions.
(define (consider-digest-match intinfo variant)
  (call/cc
   (λ (abort)
     (make-$integrity
      (equal? (integrity-info-digest intinfo) variant)
      consider-digest-match
      intinfo))))


(module+ test
  (require racket/file
           racket/function
           rackunit)
  
  ; Coverage info should flag this as uncovered when tests are passing.
  (define (fails . _)
    (fail "Control should not have reached here"))

  (define info
    (integrity-info 'sha1 #""))
  
  (define (check-integrity-passed tbd tmda decider)
    (check-equal? (check-integrity #:trust-bad-digest tbd
                                   #:trust-message-digest-algorithms tmda
                                   info
                                   #"")
                  ($integrity #t (object-name decider) info)))
  

  (test-case "Trust only certain CHFs"
    (check-true (consider-chf-trust #:trust-message-digest-algorithms '(sha1)
                                    info
                                    (λ () #t)))
    (check-integrity-passed #f '(sha1) consider-digest-match))

  
  (test-equal? "Distrust uncited CHFs"
               (consider-chf-trust #:trust-message-digest-algorithms '(md5)
                                   info
                                   fails)
               ($integrity #f (object-name consider-chf-trust) info))


  (test-case "Skip integrity checking"
    (check-equal? (consider-digest-trust #:trust-bad-digest #t info fails)
                  ($integrity #t (object-name consider-digest-trust) info))
    (check-integrity-passed #t null consider-digest-trust))

  
  (test-true "Proceed with integrity checking"
             (consider-digest-trust #:trust-bad-digest #f info (λ () #t)))
  
  (test-true (format "Count ~s as valid integrity info" info)
             (consider-integrity-info #:trust-unknown-digest #f info (const #t)))
  
  (define (test-unknown v)
    (define (check trust?)
      (equal? (consider-integrity-info #:trust-unknown-digest trust? v fails)
              ($integrity trust? (object-name consider-integrity-info) v)))

    (test-true (format "Count ~s as undeclared integrity info" v)
               (and (check #t)
                    (check #f))))

  (test-unknown 'garbage)
  (test-unknown (integrity-info "" #"a"))
  (test-unknown (integrity-info 'garbage "a")))
