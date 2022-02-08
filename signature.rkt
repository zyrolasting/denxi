#lang racket/base

; Authenticate source of bytes

(require racket/contract
         "integrity.rkt"
         "message.rkt"
         "source.rkt")


(provide (struct-out signature)
         (contract-out
          [snake-oil-signature
           (-> bytes? symbol? signature?)]
          [snake-oil-public-key-integrity
           (-> symbol? well-formed-integrity?)]
          [check-signature
           (-> #:trust-public-key? (-> input-port? any/c)
               #:verify-signature verify-signature/c
               #:trust-unsigned any/c
               #:trust-bad-digest any/c
               (or/c #f signature?)
               (or/c #f integrity?)
               symbol?)]
          [make-signature/c
           chaperone-contract?]
          [raw-signature?
           flat-contract?]
          [signature-check-passed?
           flat-contract?]
          [verify-signature/c
           chaperone-contract?]))


(define make-signature/c
  (-> bytes? symbol? bytes? (or/c #f bytes?) bytes?))


(define verify-signature/c
  (-> bytes? symbol? bytes? bytes? boolean?))


(struct signature (public-key body))


(define signature-check-passed?
  (and/c symbol?
         (or/c 'signature-verified
               'skip
               'skip-unsigned)))

(define (signature-check #:skip? skip?
                         #:trust-unsigned? trust-unsigned?
                         #:trust-public-key? trust-pk?
                         #:verify-signature verify
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


(define (snake-oil-signature digest chf)
  (signature snake-oil-public-key
             (make-signature digest
                             chf
                             snake-oil-private-key
                             snake-oil-private-key-password)))


(define (snake-oil-public-key-integrity chf-name)
  (integrity chf (make-digest snake-oil-public-key)))


(module+ test
  (require rackunit)

  (define intinfo (integrity 'snake-oil #""))
  (define siginfo (signature #"pubkey" #"body"))
  (define unsigned (signature #"pubkey" #f))
  (define (T . _) #t)
  (define (F . _) #f)

  (define pubkey-bytes #"pubkey")
  (define digest (call-with-snake-oil-chf-trust (λ () (make-digest #"abc"))))
  (define intinfo (integrity 'snake-oil digest))
  (define signature-bytes #"sig"))
