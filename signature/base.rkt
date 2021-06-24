#lang racket/base

(require racket/contract)

(provide (struct-out signature-info)
         (struct-out $signature)
         (contract-out
          [check-signature
           (-> #:trust-public-key? (-> input-port? any/c)
               #:trust-unsigned any/c
               #:trust-bad-digest any/c
               (or/c #f well-formed-signature-info/c)
               (or/c #f well-formed-integrity-info/c)
               $signature?)]
          [current-make-signature
           (parameter/c (-> bytes? symbol? bytes? (or/c #f bytes?) bytes?))]
          [current-verify-signature
           (parameter/c (-> bytes? symbol? bytes? bytes? boolean?))]
          [well-formed-signature-info/c
           flat-contract?]))


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


(define-message $signature
  (ok? stage public-key-path))


(struct signature-info (pubkey body)
  #:prefab)


(define well-formed-signature-info/c
  (struct/c signature-info bytes? bytes?))


(define (check-signature #:trust-bad-digest trust-bad-digest
                         #:trust-unsigned trust-unsigned
                         #:trust-public-key? trust?
                         intinfo
                         siginfo)
  (if trust-bad-digest
      ($signature #t 'consider-integrity-trust #f)
      (if (well-formed-signature-info/c siginfo)
          (let ([public-key (signature-info-pubkey siginfo)])
            (if (trust? (open-input-bytes public-key))
                ($signature
                 ((current-verify-signature)
                  (integrity-info-digest intinfo)
                  (integrity-info-algorithm intinfo)
                  (signature-info-body siginfo)
                  (signature-info-pubkey siginfo))
                 'consider-signature
                 #f)
                ($signature #f
                            'consider-public-key-trust
                            public-key)))
          ($signature trust-unsigned 'consider-signature-info #f))))




; The following tests are most valuable when viewed with coverage information.
; If consider-* procedure expressions are uncovered with a zero-trust configuration,
; then this is a massive red flag even if all tests pass.
;
; TODO: Raise a test failure for uncovered expressions, so that coverage does
; not need to be checked manually.

(module+ test
  (require rackunit)

  (define intinfo (integrity-info 'snake-oil #""))
  (define siginfo (signature-info #"pubkey" #"body"))
  (define unsigned (signature-info #"pubkey" #f))
  (define (T . _) #t)
  (define (F . _) #f)
  
  (test-equal? "Skip signature checking if user trusts bad digests"
               (check-signature
                #:trust-public-key? F
                #:trust-unsigned #f
                #:trust-bad-digest #t
                intinfo siginfo)
               ($signature #t 'consider-integrity-trust #f))

  (test-equal? "Continue when user does not trust bad digests"
               (check-signature
                #:trust-public-key? F
                #:trust-unsigned #f
                #:trust-bad-digest #f
                intinfo
                siginfo)
               ($signature #f 'consider-public-key-trust #"pubkey"))

  (test-equal? "Trust unsigned inputs if instructed, but announce doing so"
               (check-signature
                #:trust-public-key? F
                #:trust-unsigned #t
                #:trust-bad-digest #f
                intinfo
                unsigned)
               ($signature #t 'consider-signature-info #f))

  (test-equal? "Detect missing signatures"
               (check-signature
                #:trust-public-key? F
                #:trust-unsigned #f
                #:trust-bad-digest #f
                intinfo
                unsigned)
               ($signature #f 'consider-signature-info #f))

  (test-equal? "Do not implicitly trust any public key"
               (check-signature
                #:trust-public-key? F
                #:trust-unsigned #f
                #:trust-bad-digest #f
                intinfo
                siginfo)
               ($signature #f 'consider-public-key-trust #"pubkey"))

    (test-equal? "Distrust all signatures by default"
                 (check-signature
                  #:trust-public-key? T
                  #:trust-unsigned #f
                  #:trust-bad-digest #f
                  intinfo
                  siginfo)
                 ($signature #f 'consider-signature #f))

  
    (parameterize ([current-verify-signature T])
      (test-equal? "Verify signature"
                   (check-signature
                    #:trust-public-key? T
                    #:trust-unsigned #f
                    #:trust-bad-digest #f
                    intinfo
                    siginfo)
                   ($signature #t 'consider-signature #f))))
