#lang racket/base

; Authenticate source of bytes

(require racket/contract
         "crypto.rkt"
         "file.rkt"
         "integrity.rkt"
         "integrity/ffi.rkt"
         "state.rkt"
         "message.rkt"
         "setting.rkt"
         "signature/base.rkt"
         "signature/snake-oil.rkt"
         "source.rkt")

(provide
 (all-from-out "signature/base.rkt"
               "signature/snake-oil.rkt")
 (contract-out
  [MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH
   budget/c]
  [signature
   (-> source-variant?
       source-variant?
       signature?)]
  [fetch-signature-payload
   (-> source-variant?
       exhaust/c
       bytes?)]
  [lock-signature
   (->* ((or/c signature? signature?))
        (#:public-key-budget (or/c +inf.0 exact-nonnegative-integer?)
         #:signature-budget (or/c +inf.0 exact-nonnegative-integer?)
         exhaust/c)
        signature?)]
  [make-snake-oil-signature
   (-> bytes? symbol? signature?)]
  [verify-signature
   (case-> (-> (or/c signature?
                     signature?)
               (or/c integrity?
                     integrity?)
               symbol?)
           (-> bytes?
               symbol?
               bytes?
               bytes?
               symbol?))]
  [call-with-snake-oil-cipher-trust
   (-> (-> any) any)]
  [make-signature
   (->* (bytes? symbol? bytes?)
        ((or/c #f bytes?))
        bytes?)]))


(define+provide-setting XIDEN_TRUST_ANY_PUBLIC_KEY boolean? #f)
(define+provide-setting XIDEN_TRUST_BAD_SIGNATURE boolean? #f)
(define+provide-setting XIDEN_TRUST_PUBLIC_KEYS
  (listof (or/c integrity? integrity?)) null)
(define+provide-setting XIDEN_TRUST_UNSIGNED boolean? #f)

(struct signature (pubkey body))

(define MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH 24000)

(define verify-signature
  (case-lambda
    [(digest chf pubkey body)
     (verify-signature (integrity chf digest)
                       (signature pubkey body))]
    [(siginfo intinfo)
     (let ([trust-public-key?
            (if (XIDEN_TRUST_ANY_PUBLIC_KEY)
                (λ (p) #t)
                (bind-trust-list (XIDEN_TRUST_PUBLIC_KEYS)))])
       (check-signature #:trust-public-key? trust-public-key?
                        #:trust-signature? (current-verify-signature)
                        #:trust-unsigned (XIDEN_TRUST_UNSIGNED)
                        #:trust-bad-digest (XIDEN_TRUST_BAD_DIGEST)
                        (lock-signature siginfo)
                        (lock-integrity intinfo)))]))


(define (make-snake-oil-signature digest chf)
  (signature snake-oil-public-key
             (make-signature digest
                             chf
                             snake-oil-private-key
                             snake-oil-private-key-password)))


(define (call-with-snake-oil-cipher-trust f)
  (call-with-snake-oil-chf-trust
   (λ ()
     (XIDEN_TRUST_PUBLIC_KEYS
      (list (integrity (get-default-chf)
                       (make-digest snake-oil-public-key)))
      f))))


(define (make-signature . xs)
  (apply (current-make-signature) xs))


(define (fetch-signature-payload source-variant exhaust)
  (let ([source (coerce-source source-variant)])
    (fetch source
           (λ (in est-size)
             (file->bytes
              (build-workspace-path
               (path-record-path
                (make-addressable-file
                 #:cache-key (make-source-key source)
                 #:max-size MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH
                 #:buffer-size MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH
                 #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
                 #:on-status void
                 "_"
                 in
                 est-size)))))
           exhaust)))


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
      (and (signature-pubkey siginfo)
           (lock-source (signature-pubkey siginfo)
                        public-key-budget
                        exhaust*))
      (and (signature-body siginfo)
           (lock-source (signature-body siginfo)
                        signature-budget
                        exhaust*))))))


(module+ test
  (require rackunit
           (submod "state.rkt" test)
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
