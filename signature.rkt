#lang racket/base

(require "contract.rkt")

(provide (struct-out signature-info)
         (contract-out
          [bind-trusted-public-keys
           (-> (listof well-formed-integrity-info/c)
               (-> path-string? boolean?))]
          [well-formed-signature-info/c
           flat-contract?]
          [get-public-key-path
           (-> (or/c path-string? bytes?)
               path?)]
          [check-signature
           (-> bytes?
               path-string?
               (or/c path-string? bytes?)
               boolean?)]
          [consider-trust
           (-> #:trust-bad-digest any/c
               any/c
               (-> any/c any)
               any)]
          [consider-unsigned
           (-> #:trust-unsigned any/c
               any/c
               (-> any/c any)
               any)]
          [consider-public-key-trust
           (-> #:trust-public-key? (-> path-string? any/c)
               well-formed-signature-info/c
               (-> path-string? well-formed-signature-info/c any)
               any)]
          [consider-signature-info
           (-> path-string?
               well-formed-integrity-info/c
               well-formed-signature-info/c
               (or/c (λ (e) (or (eq? e $signature-verified)
                                (eq? e $signature-mismatch)))))]))




(require racket/sequence
         racket/format
         racket/port
         "codec.rkt"
         "file.rkt"
         "integrity.rkt"
         "message.rkt"
         "openssl.rkt"
         "url.rkt")


(define+provide-message $signature-status               (input-name input-source))
(define+provide-message $signature-missing              $signature-status ())
(define+provide-message $signature-unchecked            $signature-status ())
(define+provide-message $signature-trust-unsigned       $signature-status ())
(define+provide-message $signature-verified             $signature-status ())
(define+provide-message $signature-mismatch             $signature-status ())
(define+provide-message $signature-distrust-public-key  $signature-status (public-key-path))


(struct signature-info (pubkey body) #:prefab)

(define well-formed-signature-info/c
  (struct/c signature-info
            (or/c bytes? string?)
            (or/c bytes? string?)))


(define ESTIMATED_SIGNATURE_AND_PUBKEY_MAX_SIZE (* 100 1024))

(define (bind-trusted-public-keys trusted)
  (λ (public-key-path)
    (for/or ([integrity trusted])
      (check-integrity integrity public-key-path))))

(define (get-public-key-path variant)
  (get-cached-file* variant ESTIMATED_SIGNATURE_AND_PUBKEY_MAX_SIZE))

(define (check-signature digest public-key-path signature-variant)
  (with-handlers ([exn:fail:xiden:openssl?
                   (λ (e) (if (regexp-match? #rx"Signature Verification Failure"
                                             (exn:fail:xiden:openssl-output e))
                              #f
                              (raise e)))])

    (regexp-match?
     #rx#"Success"
     (run-openssl-command (open-input-bytes digest)
                          "pkeyutl"
                          "-verify"
                          "-sigfile" (get-cached-file* signature-variant ESTIMATED_SIGNATURE_AND_PUBKEY_MAX_SIZE)
                          "-pubin"
                          "-inkey" public-key-path))))




; All of the below consider-* procedures cooperate to select an
; $input-signature-* constructor using CPS to keep cyclomatic
; complexity low and aid testing. verify-signature-info applies
; these procedures with runtime configuration values.
;
; The more consider-* procedures used, the safer the transaction.

(define (consider-trust #:trust-bad-digest trust-bad-digest siginfo k)
  (if trust-bad-digest
      $signature-unchecked
      (k siginfo)))

(define (consider-unsigned #:trust-unsigned trust-unsigned siginfo k)
  (if (well-formed-signature-info/c siginfo)
      (k siginfo)
      (if trust-unsigned
          $signature-trust-unsigned
          $signature-missing)))

(define (consider-public-key-trust #:trust-public-key? trust-public-key? siginfo k)
  (define public-key-path (get-public-key-path (signature-info-pubkey siginfo)))
  (if (trust-public-key? public-key-path)
      (k public-key-path siginfo)
      (λ (n s) ($signature-distrust-public-key n s public-key-path))))

(define (consider-signature-info public-key-path intinfo siginfo)
  (if (check-signature (integrity-info-digest intinfo) public-key-path (signature-info-body siginfo))
      $signature-verified
      $signature-mismatch))


; The following tests are most valuable when viewed with coverage information.
; If consider-* procedure expressions are uncovered with a zero-trust configuration,
; then this is a massive red flag even if all tests pass.
;
; TODO: Raise a test failure for uncovered expressions, so that coverage does
; not need to be checked manually.
(module+ test
  (require rackunit
           (submod "file.rkt" test))

  ; The hard-coded dummy data for this test came from hashing the byte string #"abc"
  ; with SHA-384 and then signing the digest using a 2048-bit RSA private key.
  (define pubkey-bytes
    (bytes-append
     #"-----BEGIN PUBLIC KEY-----\n"
     #"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwl4mLPKcw6MW+YkIdQaY\n"
     #"oQ9vc6qBjzzoadzM3staJjzyu+secjuWo4iYrS2ORWnU/RJK8/ogapKnXo9N7aVw\n"
     #"K3nkvvaEnBsJ6IqW6/OvUlc8x09t4mOX8HVyxMJNATiaNBvPqC800cvU9Do2QQjb\n"
     #"on2ZoBqtXcpOqwi2I/GNhzWwSuxiUzGlv28GHzV+H8o8s71XuWzCGxm2U71xBKrT\n"
     #"prvz9aTIcmGiGf1VJ3u2QdLwyq1HZn6e8NA8iszUkUPkSDeEt3GnnlMhJWZAVG9C\n"
     #"U5UwNFhoaLC+rpU6BFxHkPDY7pGYoOiE564CS8Ahg+ssRO9BMVdgfJvk+VrM662B\n"
     #"FwIDAQAB\n"
     #"-----END PUBLIC KEY-----\n"))

  (define signature-bytes
    (bytes-append
     #"7i:3\277\261\312>\34d05\252\273\37\333\341\356\277\340\220\206"
     #"\302\357qY\325\362\0235\321\365b\17\274\321\303\275LI\263L\256"
     #"?\270\362<\351<x\232?W\23X`R\32\372\n\265\n\252C\355\231\32\220"
     #"\314U\245\311\301\261\177W\214\377ZN\276\212\335\177\355\21*\332"
     #"\"\372pNE\221\223u!l\356\247\355{\320fH\a\340@\331\251\335uU\r"
     #"\347t\306!\240O]h<\21U<H\317\310G\4\275\324o\6\351\1'.\320\372"
     #"\275V\266i:$\322\212\0\233\0\235\n\t\343\3252\221\2\16}\255\337"
     #"j?\252N]\330\307\247S1k\250\361W\335\251\310\200\275\r\230,\313"
     #"\270=}\244%?\342\350\353\2z\305:=\330\352b\327,\255\vlK>9\340"
     #"\16\371\360|\373\263\312\246\4:\360\373\31\353\216T\3@\260X\20"
     #"\375\272\3{\276e\237\205L\316\315\341\252\266b\221\240\3310J\21\\"))

  (define intinfo
    (integrity-info 'sha384
     (bytes-append #"\313\0u?E\243^\213\265\240=i\232\306P\a',2"
                   #"\253\16\336\321c\32\213`ZC\377[\355\200\206"
                   #"\a+\241\347\314#X\272\354\2414\310%\247")))

  ; The content used for the integrity info does not matter. All
  ; that matters is if the signature matches based on it.
  (define siginfo
    (signature-info pubkey-bytes signature-bytes))

  ; Coverage info should flag this as uncovered when tests are passing.
  (define (fails . _)
    (fail "Control should not have reached here"))

  ; A new workspace controls /tmp file pollution
  (test-workspace "Verify signatures"
    (test-eq? "Skip signature checking if user trusts bad digests"
              (consider-trust #:trust-bad-digest #t siginfo fails)
              $signature-unchecked)

    (test-eq? "Continue when user does not trust bad digests"
              (consider-trust #:trust-bad-digest #f siginfo values)
              siginfo)

    (test-eq? "Trust unsigned inputs if instructed, but announce doing so"
              (consider-unsigned #:trust-unsigned #t #f fails)
              $signature-trust-unsigned)

    (test-case "Detect missing signatures"
      (check-eq? (consider-unsigned #:trust-unsigned #f #f fails) $signature-missing)
      (check-eq? (consider-unsigned #:trust-unsigned #f (signature-info #"" #f) fails) $signature-missing))

    (test-eq? "Continue if user does not trust unsigned inputs, and a signature is present"
              (consider-unsigned #:trust-unsigned #f siginfo values)
              siginfo)

    (test-case "Do not implicitly trust any public key"
      (define ctor (consider-public-key-trust #:trust-public-key? (λ (p) #f) siginfo fails))
      (check-match (ctor 1 2)
                   ($signature-distrust-public-key 1 2 (? path? _))))

    (test-case "Continue when trusting a public key"
      (define trust-public-key?
        (bind-trusted-public-keys
         (list (integrity-info 'sha384
                               (make-digest pubkey-bytes 'sha384)))))

      (define-values (public-key-path s)
        (consider-public-key-trust #:trust-public-key? trust-public-key? siginfo values))

      (check-pred file-exists? public-key-path)
      (check-eq? s siginfo)

      (test-eq? "Find valid signature"
                (consider-signature-info public-key-path intinfo siginfo)
                $signature-verified)

      (test-eq? "Catch tampered integrity as signature mismatch"
                (consider-signature-info public-key-path (integrity-info 'sha384 #"different") siginfo)
                $signature-mismatch)

      (test-eq? "Catch tampered signature as signature mismatch"
                (consider-signature-info public-key-path intinfo (signature-info pubkey-bytes #"different"))
                $signature-mismatch)

      (test-exn "Don't hide OpenSSL errors"
                exn:fail:xiden:openssl?
                (λ () (consider-signature-info "garbage!" intinfo siginfo))))))
