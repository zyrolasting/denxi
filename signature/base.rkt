#lang racket/base

(require racket/contract)

(provide
 (struct-out signature)
 (contract-out
  [check-signature
   (-> #:trust-public-key? (-> input-port? any/c)
       #:verify-signature verify-signature/c
       #:trust-unsigned any/c
       #:trust-bad-digest any/c
       (or/c #f signature?)
       (or/c #f integrity?)
       symbol?)]
  [current-make-signature
   (parameter/c make-signature/c)]
  [current-verify-signature
   (parameter/c verify-signature/c)]
  [make-signature/c
   chaperone-contract?]
  [raw-signature?
   flat-contract?]
  [signature-check-passed?
   flat-contract?]
  [verify-signature/c
   chaperone-contract?]))

(require "../integrity.rkt"
         "../message.rkt"
         "ffi.rkt")

(define make-signature/c
  (-> bytes? symbol? bytes? (or/c #f bytes?) bytes?))

(define verify-signature/c
  (-> bytes? symbol? bytes? bytes? boolean?))

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

(define signature-check-passed?
  (and/c symbol?
         (or/c 'signature-verified
               'skip
               'skip-unsigned)))

(define (check-signature #:trust-bad-digest trust-bad-digest
                         #:trust-unsigned trust-unsigned
                         #:trust-public-key? trust-pk?
                         #:verify-signature trust-sig?
                         siginfo
                         intinfo)
  (if trust-bad-digest
      'skip
      (if (and (raw-signature? siginfo)
               (raw-integrity? intinfo))
          (let ([public-key (signature-public-key siginfo)])
            (if (trust-pk? (open-input-bytes public-key))
                (if (trust-sig?
                     (integrity-digest intinfo)
                     (integrity-chf-symbol intinfo)
                     (signature-body siginfo)
                     public-key)
                    'signature-verified
                    'signature-unverified)
                'blocked-public-key))
          (if trust-unsigned
              'skip-unsigned
              'unsigned))))


(module+ test
  (require rackunit)

  (define intinfo (integrity 'snake-oil #""))
  (define siginfo (signature #"pubkey" #"body"))
  (define unsigned (signature #"pubkey" #f))
  (define (T . _) #t)
  (define (F . _) #f)

  (check-true  (signature-check-passed? 'skip))
  (check-true  (signature-check-passed? 'signature-verified))
  (check-true  (signature-check-passed? 'skip-unsigned))
  (check-false (signature-check-passed? 'blocked-public-key))
  (check-false (signature-check-passed? 'unsigned))
  (check-false (signature-check-passed? 'signature-unverified))

  (check-eq? (check-signature
              #:trust-public-key? F
              #:verify-signature F
              #:trust-unsigned #f
              #:trust-bad-digest #t
              siginfo
              intinfo)
             'skip)

  (check-eq? (check-signature
              #:trust-public-key? F
              #:verify-signature F
              #:trust-unsigned #f
              #:trust-bad-digest #f
              siginfo
              intinfo)
             'blocked-public-key)

  (check-eq? (check-signature
              #:trust-public-key? T
              #:verify-signature F
              #:trust-unsigned #f
              #:trust-bad-digest #f
              siginfo
              intinfo)
             'signature-unverified)
  
  (check-eq? (check-signature
              #:trust-public-key? T
              #:verify-signature T
              #:trust-unsigned #f
              #:trust-bad-digest #f
              siginfo
              intinfo)
             'signature-verified))
