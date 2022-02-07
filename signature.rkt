#lang racket/base

; Authenticate source of bytes

(require racket/contract
         "crypto.rkt"
         "integrity.rkt"
         "integrity/ffi.rkt"
         "message.rkt"
         "signature/base.rkt"
         "signature/snake-oil.rkt"
         "source.rkt")


(provide (all-from-out "signature/base.rkt"
                       "signature/snake-oil.rkt")
         (contract-out
          [MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH
           budget/c]
          [lock-signature
           (->* (signature?)
                (#:public-key-budget budget/c
                 #:signature-budget budget/c
                 exhaust/c)
                signature?)]
          [snake-oil-signature
           (-> bytes? symbol? signature?)]
          [snake-oil-public-key-integrity
           (-> symbol? well-formed-integrity?)]
          [make-signature
           (->* (bytes? symbol? bytes?)
                ((or/c #f bytes?))
                bytes?)]
          [malformed-signature?
           flat-contract?]
          [sourced-signature?
           flat-contract?]
          [well-formed-signature?
           flat-contract?]))

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


(define (sourced-signature? v)
  (and (signature? v)
       (or (source? (signature-body v))
           (source? (signature-public-key v)))))

(define well-formed-signature?
  (or/c raw-signature? sourced-signature?))

(define malformed-signature?
  (not/c well-formed-signature?))

; TODO: Verify if this is reasonable.
(define MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH
  24000)

(define (snake-oil-signature digest chf)
  (signature snake-oil-public-key
             (make-signature digest
                             chf
                             snake-oil-private-key
                             snake-oil-private-key-password)))


(define (snake-oil-public-key-integrity chf)
  (integrity chf (make-digest snake-oil-public-key chf)))


(define (make-signature . xs)
  (apply (current-make-signature) xs))



(define (lock-signature siginfo
                        #:public-key-budget
                        [public-key-budget MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                        #:signature-budget
                        [signature-budget MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                        [exhaust raise])
  (let/cc abort
    (define (exhaust* v)
      (abort (exhaust v)))
    (signature
     (and (signature-public-key siginfo)
          (lock-source (signature-public-key siginfo)
                       public-key-budget
                       exhaust*))
     (and (signature-body siginfo)
          (lock-source (signature-body siginfo)
                       signature-budget
                       exhaust*)))))


(module+ test
  (require rackunit
           (submod "integrity.rkt" test))

  (define pubkey-bytes #"pubkey")
  (define digest (call-with-snake-oil-chf-trust (λ () (make-digest #"abc"))))
  (define intinfo (integrity 'snake-oil digest))
  (define signature-bytes #"sig")

  ; The content used for the integrity info does not matter. All
  ; that matters is if the signature matches based on it.
  (define siginfo
    (signature pubkey-bytes signature-bytes))
  
  (test-case "Lock signature info"
    (define example (signature (text-source "wx") (text-source "yz")))

    (define (try pb sb)
      (lock-signature #:public-key-budget pb
                      #:signature-budget sb
                      example))

    (check-match (try 0 0) (signature (text-source "wx") (text-source "yz")))
    (check-match (try 2 2) (signature #"wx" #"yz"))
    (check-match (try 0 2) (signature (text-source "wx") #"yz"))
    (check-match (try 2 0) (signature #"wx" (text-source "yz")))

    (test-case "Exhaust a lock on first lock-signature failure"
      (define (try-exhaust p s)
        (check-equal? (lock-signature (signature p s)
                                      values)
                      1))
      (try-exhaust (exhausted-source 1) #"")
      (try-exhaust #"" (exhausted-source 1))
      (try-exhaust (exhausted-source 1) (exhausted-source 2)))))
