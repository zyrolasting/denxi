#lang racket/base

; Authenticate source of bytes.

(require "contract.rkt"
         "file.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "openssl.rkt"
         "plugin.rkt"
         "source.rkt"
         "strict-rc.rkt"
         "workspace.rkt")

(provide (struct-out signature-info)
         (contract-out
          [make-signature-bytes
           (-> bytes? path-string? (or/c #f path-string?) bytes?)]
          [signature
           (-> source-variant?
               source-variant?
               signature-info?)]
          [well-formed-signature-info/c
           flat-contract?]
          [fetch-signature-payload
           (-> source-variant? exhaust/c path-string?)]
          [check-signature
           (-> #:trust-public-key? (-> path-string? any/c)
               #:public-key-path path-string?
               #:trust-unsigned any/c
               #:trust-bad-digest any/c
               (or/c #f well-formed-signature-info/c)
               (or/c #f well-formed-integrity-info/c)
               $signature?)]))

(define+provide-message $signature (ok? stage public-key-path))

(struct signature-info (pubkey body) #:prefab)

(define signature signature-info)

(define well-formed-signature-info/c
  (struct/c signature-info
            source-variant?
            source-variant?))

(define MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH 24000)

(define (default-verify-signature intinfo siginfo)
  (with-handlers ([exn:fail:xiden:openssl?
                   (λ (e) (if (regexp-match? #rx"Signature Verification Failure"
                                             (exn:fail:xiden:openssl-output e))
                              #f
                              (raise e)))])
    (regexp-match?
     #rx#"Success"
     (run-openssl-command (open-input-bytes (fetch-digest intinfo raise))
                          "pkeyutl"
                          "-verify"
                          "-sigfile" (fetch-signature-payload (signature-info-body siginfo) raise)
                          "-pubin"
                          "-inkey" (fetch-signature-payload (signature-info-pubkey siginfo) raise)))))


(define (make-signature-bytes digest private-key-path password-path)
  (define base-args
    (list "pkeyutl"
          "-sign"
          "-inkey" private-key-path))
  (apply run-openssl-command #:timeout +inf.0
         (open-input-bytes digest)
         (if password-path
             (append base-args
                     (list "-passin" (format "file:~a" password-path)))
             base-args)))



(define (fetch-signature-payload source-variant exhaust)
  (let ([source (coerce-source source-variant)])
    (fetch source
           (λ (in est-size)
             (build-workspace-path
              (path-record-path
               (make-addressable-file
                #:cache-key (make-source-key source)
                #:max-size MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH
                #:buffer-size MAX_EXPECTED_SIGNATURE_PAYLOAD_LENGTH
                #:timeout-ms (rc-ref 'XIDEN_FETCH_TIMEOUT_MS)
                #:on-status void
                "_"
                in
                est-size))))
           exhaust)))


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
                                   #:public-key-path public-key-path siginfo k)
  (if (trust-public-key? public-key-path)
      (k public-key-path siginfo)
      ($signature #f
                  (object-name consider-public-key-trust)
                  (and (file-exists? public-key-path)
                       (file->bytes public-key-path)))))

(define (consider-signature intinfo siginfo)
  ($signature ((plugin-ref 'verify-signature default-verify-signature) intinfo siginfo)
              (object-name consider-signature)
              #f))


(define (check-signature #:trust-public-key? trust-public-key?
                         #:public-key-path public-key-path
                         #:trust-unsigned trust-unsigned
                         #:trust-bad-digest trust-bad-digest
                         siginfo
                         intinfo)
  (consider-integrity-trust #:trust-bad-digest trust-bad-digest siginfo
   (λ _
     (consider-signature-info #:trust-unsigned trust-unsigned siginfo
      (λ _
        (consider-public-key-trust #:trust-public-key? trust-public-key? #:public-key-path public-key-path siginfo
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
    (test-equal? "Skip signature checking if user trusts bad digests"
                 (consider-integrity-trust #:trust-bad-digest #t siginfo fails)
                 ($signature #t (object-name consider-integrity-trust) #f))

    (test-equal? "Continue when user does not trust bad digests"
                 (consider-integrity-trust #:trust-bad-digest #f siginfo values)
                 siginfo)

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
                                            #:public-key-path "/tmp/junk" siginfo fails)
                 ($signature #f (object-name consider-public-key-trust) #f))

    (test-case "Continue when trusting a public key"
      (define trust-public-key?
        (bind-trust-list
         (list (integrity-info 'sha384
                               (make-digest pubkey-bytes 'sha384)))))

      (define-values (public-key-path s)
        (rc-rebind 'XIDEN_TRUST_MESSAGE_DIGEST_ALGORITHMS '(sha384)
                   (λ ()
                     (consider-public-key-trust
                      #:public-key-path
                      (fetch-signature-payload (signature-info-pubkey siginfo)
                                               (λ (e) (values e e)))
                      #:trust-public-key? trust-public-key?
                      siginfo
                      values))))

      (check-pred file-exists? public-key-path)
      (check-eq? s siginfo)

      (test-equal? "Find valid signature"
                   (consider-signature intinfo siginfo)
                   ($signature #t (object-name consider-signature) #f))

      (test-equal? "Catch tampered integrity as signature mismatch"
                   (consider-signature (integrity-info 'sha384 #"different") siginfo)
                   ($signature #f (object-name consider-signature) #f))

      (test-equal? "Catch tampered signature as signature mismatch"
                   (consider-signature intinfo (signature-info pubkey-bytes #"different"))
                   ($signature #f (object-name consider-signature) #f))

      (test-exn "Don't hide OpenSSL errors"
                exn:fail:xiden:openssl?
                (λ () (consider-signature intinfo
                                          (struct-copy signature-info siginfo
                                                       [pubkey #"garbage"])))))))
