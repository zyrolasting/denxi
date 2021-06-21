#lang racket/base

; Authenticate source of bytes.

(require racket/contract
         "crypto.rkt"
         "file.rkt"
         "integrity.rkt"
         "state.rkt"
         "message.rkt"
         "setting.rkt"
         "source.rkt")

(provide (struct-out signature-info)
         (contract-out
          [MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH
           budget/c]
          [current-verify-signature
           (parameter/c
            (-> well-formed-integrity-info/c well-formed-signature-info/c boolean?))]
          [signature
           (-> source-variant?
               source-variant?
               signature-info?)]
          [well-formed-signature-info/c
           flat-contract?]
          [fetch-signature-payload
           (-> source-variant? exhaust/c bytes?)]
          [check-signature
           (-> #:trust-public-key? (-> input-port? any/c)
               #:public-key bytes?
               #:trust-unsigned any/c
               #:trust-bad-digest any/c
               (or/c #f well-formed-signature-info/c)
               (or/c #f well-formed-integrity-info/c)
               $signature?)]
          [lock-signature-info
           (->* (well-formed-signature-info/c)
                (#:public-key-budget (or/c +inf.0 exact-nonnegative-integer?)
                 #:signature-budget (or/c +inf.0 exact-nonnegative-integer?)
                 exhaust/c)
                well-formed-signature-info/c)]
          [make-snake-oil-signature-info
           (->* (bytes?)
                (chf/c)
                well-formed-signature-info/c)]
          [call-with-trust-in-snake-oil
           (-> (-> any) any)]
          [call-with-faith-in-snake-oil
           (-> (-> any) any)]))
          

(define+provide-message $signature (ok? stage public-key-path))

(define+provide-setting XIDEN_TRUST_ANY_PUBLIC_KEY boolean? #f)
(define+provide-setting XIDEN_TRUST_BAD_SIGNATURE boolean? #f)
(define+provide-setting XIDEN_TRUST_PUBLIC_KEYS (listof well-formed-integrity-info/c) null)
(define+provide-setting XIDEN_TRUST_UNSIGNED boolean? #f)

(struct signature-info (pubkey body) #:prefab)

(define signature signature-info)

(define well-formed-signature-info/c
  (struct/c signature-info
            source-variant?
            source-variant?))

(define MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH 24000)


(define (default-verify-signature intinfo siginfo)
  (verify-signature (fetch-digest intinfo raise)
                    (integrity-info-algorithm intinfo)
                    (fetch-signature-payload (signature-info-body siginfo) raise)
                    (fetch-signature-payload (signature-info-pubkey siginfo) raise)))


(define current-verify-signature
  (make-parameter default-verify-signature))


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


(define (lock-signature-info siginfo
                             #:public-key-budget
                             [public-key-budget MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                             #:signature-budget
                             [signature-budget MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH]
                             [exhaust raise])
  (call/cc
   (λ (abort)
     (define (exhaust* v)
       (abort (exhaust v)))
     (signature-info
      (lock-source (signature-info-pubkey siginfo)
                   public-key-budget
                   exhaust*)
      (lock-source (signature-info-body siginfo)
                   signature-budget
                   exhaust*)))))


(define (make-snake-oil-signature-info digest [chf DEFAULT_CHF])
  (signature-info
   snake-oil-public-key
   (sign-with-snake-oil digest chf)))


(define (call-with-trust-in-snake-oil f)
  (XIDEN_TRUST_CHFS
   (cons DEFAULT_CHF (XIDEN_TRUST_CHFS))
   (λ ()
     (XIDEN_TRUST_PUBLIC_KEYS
      (cons (integrity DEFAULT_CHF
                       (make-digest snake-oil-public-key
                                    DEFAULT_CHF))
            (XIDEN_TRUST_PUBLIC_KEYS))
      f))))

(define (call-with-faith-in-snake-oil f)
  (XIDEN_TRUST_CHFS
   (list DEFAULT_CHF)
   (λ ()
     (XIDEN_TRUST_PUBLIC_KEYS
      (list (integrity DEFAULT_CHF
                       (make-digest snake-oil-public-key
                                    DEFAULT_CHF)))
      f))))


; ------------------------------------------------------------------------------
; Affirmations


(define (consider-integrity-trust #:trust-bad-digest trust-bad-digest siginfo k)
  (if trust-bad-digest
      ($signature #t (object-name consider-integrity-trust) #f)
      (k siginfo)))

(define (consider-signature-info #:trust-unsigned trust-unsigned siginfo k)
  (if (well-formed-signature-info/c siginfo)
      (k siginfo)
      ($signature trust-unsigned (object-name consider-signature-info) #f)))

(define (consider-public-key-trust #:trust-public-key? trust-public-key?
                                   #:public-key public-key siginfo k)
  (if (trust-public-key? (open-input-bytes public-key))
      (k public-key siginfo)
      ($signature #f
                  (object-name consider-public-key-trust)
                  public-key)))

(define (consider-signature intinfo siginfo)
  ($signature ((current-verify-signature) intinfo siginfo)
              (object-name consider-signature)
              #f))


(define (check-signature #:trust-public-key? trust-public-key?
                         #:public-key public-key
                         #:trust-unsigned trust-unsigned
                         #:trust-bad-digest trust-bad-digest
                         siginfo
                         intinfo)
  (consider-integrity-trust #:trust-bad-digest trust-bad-digest siginfo
                            (λ _
                              (consider-signature-info #:trust-unsigned trust-unsigned siginfo
                                                       (λ _
                                                         (consider-public-key-trust #:trust-public-key? trust-public-key? #:public-key public-key siginfo
                                                                                    (λ _
                                                                                      (consider-signature intinfo siginfo))))))))



; The following tests are most valuable when viewed with coverage information.
; If consider-* procedure expressions are uncovered with a zero-trust configuration,
; then this is a massive red flag even if all tests pass.
;
; TODO: Raise a test failure for uncovered expressions, so that coverage does
; not need to be checked manually.
(module+ test
  (require rackunit
           (submod "state.rkt" test))

  (define pubkey-bytes
    snake-oil-public-key)
  
  (define digest
    (make-digest #"abc"))

  (define intinfo
    (integrity-info DEFAULT_CHF digest))

  (define signature-bytes
    (sign-with-snake-oil digest))

  ; The content used for the integrity info does not matter. All
  ; that matters is if the signature matches based on it.
  (define siginfo
    (signature-info pubkey-bytes signature-bytes))

  ; Coverage info should flag this as uncovered when tests are passing.
  (define (fails . _)
    (fail "Control should not have reached here"))


  (test-case "Lock signature info"
    (define example (signature-info (text-source "wx") (text-source "yz")))

    (define (try pb sb)
      (lock-signature-info #:public-key-budget pb
                           #:signature-budget sb
                           example))

    (check-match (try 0 0)
                 (signature-info (text-source "wx") (text-source "yz")))

    (check-match (try 2 2)
                 (signature-info #"wx" #"yz"))

    (check-match (try 0 2)
                 (signature-info (text-source "wx") #"yz"))

    (check-match (try 2 0)
                 (signature-info #"wx" (text-source "yz")))

    (test-case "Exhaust a lock on first lock-signature failure"
      (define (try-exhaust p s)
        (check-equal? (lock-signature-info (signature-info p s)
                                           values)
                      1))
      (try-exhaust (exhausted-source 1) #"")
      (try-exhaust #"" (exhausted-source 1))
      (try-exhaust (exhausted-source 1) (exhausted-source 2))))


  (test-workspace "Verify signatures"
    (test-equal? "Skip signature checking if user trusts bad digests"
                 (check-signature
                  #:trust-public-key? (λ _ #f)
                  #:public-key #f
                  #:trust-unsigned #f
                  #:trust-bad-digest #t
                  siginfo fails)
                 ($signature #t (object-name consider-integrity-trust) #f))

    (test-equal? "Continue when user does not trust bad digests"
                 (check-signature
                  #:trust-public-key? (λ _ #f)
                  #:public-key #"x"
                  #:trust-unsigned #f
                  #:trust-bad-digest #f
                  siginfo
                  fails)
                 ($signature #f (object-name consider-public-key-trust) #"x"))

    (test-equal? "Trust unsigned inputs if instructed, but announce doing so"
                 (consider-signature-info #:trust-unsigned #t #f fails)
                 ($signature #t (object-name consider-signature-info) #f))

    (test-case "Detect missing signatures"
      (check-equal? (consider-signature-info #:trust-unsigned #f #f fails)
                    ($signature #f (object-name consider-signature-info) #f))

      (check-equal? (consider-signature-info #:trust-unsigned #f  (signature-info #"" #f) fails)
                    ($signature #f (object-name consider-signature-info) #f)))

    (test-eq? "Continue if user does not trust unsigned inputs, and a signature is present"
              (consider-signature-info #:trust-unsigned #f siginfo values)
              siginfo)

    (test-equal? "Do not implicitly trust any public key"
                 (consider-public-key-trust #:trust-public-key? (λ (p) #f)
                                            #:public-key #"junk"
                                            siginfo
                                            fails)
                 ($signature #f (object-name consider-public-key-trust) #"junk"))

    (test-case "Continue when trusting a public key"
      (define trust-public-key?
        (bind-trust-list
         (list (integrity-info DEFAULT_CHF
                               (make-digest (open-input-bytes pubkey-bytes)
                                            DEFAULT_CHF)))))

      (define-values (public-key s)
        (XIDEN_TRUST_CHFS (list DEFAULT_CHF)
          (λ ()
            (consider-public-key-trust
             #:public-key
             (fetch-signature-payload (signature-info-pubkey siginfo)
                                      (λ (e) (values e e)))
             #:trust-public-key? trust-public-key?
             siginfo
             values))))

      (check-eq? s siginfo)

      (test-equal? "Find valid signature"
                   (consider-signature intinfo siginfo)
                   ($signature #t (object-name consider-signature) #f))

      (test-equal? "Catch tampered integrity as signature mismatch"
                   (consider-signature (integrity-info DEFAULT_CHF #"different") siginfo)
                   ($signature #f (object-name consider-signature) #f))

      (test-equal? "Catch tampered signature as signature mismatch"
                   (consider-signature intinfo (signature-info pubkey-bytes #"different"))
                   ($signature #f (object-name consider-signature) #f)))))
