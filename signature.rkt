#lang racket/base

; Authenticate source of bytes

(require racket/contract
         "crypto.rkt"
         "file.rkt"
         "integrity.rkt"
         "integrity/ffi.rkt"
         "message.rkt"
         "signature/base.rkt"
         "signature/snake-oil.rkt"
         "source.rkt")

(provide
 (all-from-out "signature/base.rkt"
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
  [make-snake-oil-signature
   (-> bytes? symbol? signature?)]
  [call-with-snake-oil-cipher-trust
   (-> (-> any) any)]
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
  (integrity chf (make-digest snake-oil-public-key)))


(define (make-signature . xs)
  (apply (current-make-signature) xs))



(define (lock-signature siginfo
                        #:public-key-budget
                        [public-key-budget MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                        #:signature-budget
                        [signature-budget MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                        [exhaust raise])
  (call/cc
   (λ (abort)
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
                        exhaust*))))))


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
