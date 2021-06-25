#lang racket/base

(require racket/contract)

(provide
 (struct-out signature)
 (contract-out
  [check-signature
   (-> #:trust-public-key? (-> input-port? any/c)
       #:trust-signature? trust-signature-predicate/c
       #:trust-unsigned any/c
       #:trust-bad-digest any/c
       raw-signature?
       raw-integrity?
       symbol?)]
  [current-make-signature
   (parameter/c (-> bytes? symbol? bytes? (or/c #f bytes?) bytes?))]
  [current-verify-signature
   (parameter/c trust-signature-predicate/c)]
  [raw-signature?
   flat-contract?]))

(define trust-signature-predicate/c
  (-> bytes? symbol? bytes? bytes? boolean?))

(require "../integrity.rkt"
         "../message.rkt"
         "ffi.rkt")


(define current-verify-signature
  (make-parameter
   (if (signature-ffi-available?!)
       signature-ffi-verify-signature!
       (λ _ #f))))


(define current-make-signature
  (make-parameter
   (if (signature-ffi-available?!)
       signature-ffi-make-signature!
       (λ _ #""))))


(struct signature (public-key body)
  #:transparent)

(define raw-signature?
  (struct/c signature bytes? bytes?))


(define (check-signature #:trust-bad-digest trust-bad-digest
                         #:trust-unsigned trust-unsigned
                         #:trust-public-key? trust-pk?
                         #:trust-signature? trust-sig?
                         siginfo
                         intinfo)
  (if trust-bad-digest
      'skip
      (let ([public-key (signature-public-key siginfo)])
        (if (trust-pk? (open-input-bytes public-key))
            (if (trust-sig?
                 (integrity-digest intinfo)
                 (integrity-chf-symbol intinfo)
                 (signature-body siginfo)
                 public-key)
                'pass
                'fail)
            'curb))))


(module+ test
  (require rackunit)

  (define intinfo (integrity 'snake-oil #""))
  (define siginfo (signature #"pubkey" #"body"))
  (define unsigned (signature #"pubkey" #f))
  (define (T . _) #t)
  (define (F . _) #f)
  
  (check-eq? (check-signature
              #:trust-public-key? F
              #:trust-signature? F
              #:trust-unsigned #f
              #:trust-bad-digest #t
              intinfo
              siginfo)
             'skip)

  (check-eq? (check-signature
              #:trust-public-key? F
              #:trust-signature? F
              #:trust-unsigned #f
              #:trust-bad-digest #f
              intinfo
              siginfo)
             'curb)

  (check-eq? (check-signature
              #:trust-public-key? T
              #:trust-signature? F
              #:trust-unsigned #f
              #:trust-bad-digest #f
              intinfo
              siginfo)
             'fail)
  
  (check-eq? (check-signature
              #:trust-public-key? T
              #:trust-signature? T
              #:trust-unsigned #f
              #:trust-bad-digest #f
              intinfo
              siginfo)
             'pass))
