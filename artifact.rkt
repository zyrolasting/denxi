#lang racket/base

(require racket/contract
         "integrity.rkt"
         "signature.rkt")

(define (reimplement/c v)
  (error "Reimplement (contract)"))

(provide
 (struct-out artifact)
 (struct-out $artifact)
 (struct-out $artifact:signature)
 (struct-out $artifact:integrity)
 (contract-out
  [install-artifact
   (-> artifact?
       reimplement/c
       reimplement/c
       (subprogram/c reimplement/c))]
  [verify-artifact
   (-> artifact?
       reimplement/c
       (subprogram/c void?))]
  [fetch-artifact
   (-> string?
       artifact?
       (subprogram/c reimplement/c))]
  [make-artifact
   (->* (source-variant?)
        ((or/c #f integrity?)
         (or/c #f signature?))
        artifact?)]
  [lock-artifact
   (->* (artifact?)
        (exhaust/c
         #:content? any/c
         #:integrity? any/c
         #:signature? any/c
         #:content-budget budget/c
         #:digest-budget budget/c
         #:public-key-budget budget/c
         #:signature-budget budget/c)
        artifact?)]))


(require racket/match
         "crypto.rkt"
         "format.rkt"
         "state.rkt"
         "subprogram.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt"
         "printer.rkt"
         "source.rkt")

(define-message $artifact ())
(define-message $artifact:integrity (status chf-symbol))
(define-message $artifact:signature (status public-key))

(struct artifact
  (source      ; Defines where bytes come from
   integrity   ; Integrity information: Did I get the right bytes?
   signature)) ; Signature for authentication: Did the bytes come from someone I trust?


(define (make-artifact source [integrity #f] [signature #f])
  (artifact source integrity signature))


(define (install-artifact arti state-key reference-key)
  (mdo known := (fetch-artifact state-key arti)
       (verify-artifact arti known)
       (void)
       (subprogram-unit (cons reference-key known))))


(define (verify-artifact arti known)
  (mdo (check-artifact-integrity arti known)
       (check-artifact-signature arti known)
       (subprogram-unit (void))))


(define (fetch-artifact name arti)
  (error "Reimplement.")
  #;(state-allot (current-state)
               (coerce-source (artifact-source arti))
               (struct-copy transfer-policy zero-trust-transfer-policy
                            [max-size (mebibytes->bytes (DENXI_FETCH_TOTAL_SIZE_MB))]
                            [buffer-size (mebibytes->bytes (DENXI_FETCH_BUFFER_SIZE_MB))]
                            [timeout-ms (DENXI_FETCH_TIMEOUT_MS)]
                            [telemeter (make-on-status (current-message-formatter))])))


(define (lock-artifact #:content? [content? #t]
                       #:integrity? [integrity? #t]
                       #:signature? [signature? #t]
                       #:content-budget [content-budget (* 1024 200)]
                       #:digest-budget [digest-budget +inf.0]
                       #:public-key-budget [public-key-budget +inf.0]
                       #:signature-budget [signature-budget +inf.0]
                       arti
                       [exhaust raise])
  (call/cc
   (λ (abort)
     (define (exhaust* v)
       (abort (exhaust v)))
     (let ([lock (λ (? s c) (if (and ? s) (c s) s))])
       (artifact (lock content?
                       (artifact-source arti)
                       (λ (content)
                         (lock-source content
                                      content-budget
                                      exhaust*)))
                 (lock integrity?
                       (artifact-integrity arti)
                       (λ (intinfo)
                         (lock-integrity #:digest-budget digest-budget
                                         intinfo
                                         exhaust*)))
                 (lock signature?
                       (artifact-signature arti)
                       (λ (siginfo)
                         (lock-signature #:public-key-budget public-key-budget
                                         #:signature-budget signature-budget
                                         siginfo
                                         exhaust*))))))))


(define ((make-on-status formatter) m)
  (if ($transfer:progress? ($transfer:scope-message m))
      (printf "\r~a~a" (formatter m)
              (if (equal? ($transfer:progress-bytes-read ($transfer:scope-message m))
                          ($transfer:progress-max-size ($transfer:scope-message m)))
                  "\n" ""))
      (write-message #:newline? #f m formatter)))


(define-subprogram (check-artifact-integrity arti reference)
  (match-define (artifact content int sig) arti)
  (define-values (int/use chf)
    (if (well-formed-integrity? int)
        (values (lock-integrity int)
                (integrity-chf-symbol int))
        (values #f #f)))
  (define status
    (check-integrity
     #:trust-bad-digest (DENXI_TRUST_BAD_DIGEST)
     (make-user-chf-trust-predicate)
     int/use
     (and chf
          (make-digest (state-ref (current-state)
                                  reference
                                  (λ () ($fail #f)))
                       (integrity-chf-symbol (artifact-integrity arti))))))
  ($attach (or (integrity-check-passed? status) FAILURE)
           ($artifact:integrity status chf)))



(define-subprogram (check-artifact-signature arti path)
  (match-define (artifact content int sig) arti)
  (define sig/use
    (and (well-formed-signature? sig)
         (lock-signature sig)))
  (define int/use
    (and (well-formed-integrity? int)
         (lock-integrity int)))
  (define trust-public-key?
    (if (DENXI_TRUST_ANY_PUBLIC_KEY)
        (λ (p) #t)
        (bind-trust-list (DENXI_TRUST_PUBLIC_KEYS))))
  (define status
    (check-signature #:trust-unsigned (DENXI_TRUST_UNSIGNED)
                     #:trust-bad-digest (DENXI_TRUST_BAD_DIGEST)
                     #:trust-public-key? trust-public-key?
                     #:verify-signature (current-verify-signature)
                     sig/use
                     int/use))
  ($attach (or (signature-check-passed? status) FAILURE)
           ($artifact:signature status
                                (and (signature? sig/use)
                                     (signature-public-key sig/use)))))


(module+ test
  (require rackunit
           racket/file
           (submod "state.rkt" test)
           (submod "subprogram.rkt" test))

  (test-state "Fetch artifacts"
              (define data #"abc")
              (parameterize ([current-output-port (open-output-nowhere)])
                (call-with-snake-oil-chf-trust
                 (λ ()
                   (check-subprogram
                    (fetch-artifact "anon" (make-artifact (byte-source data)))
                    (λ (record messages)
                      (check-pred input-port? record)
                      (check-equal? (file->bytes record) data)
                      (test-case "Verify artifacts"
                        (define intinfo
                          (make-trusted-integrity data))
                        (define siginfo
                          (make-snake-oil-signature
                           (integrity-digest intinfo)
                           (integrity-chf-symbol intinfo)))
                        (define arti
                          (artifact (byte-source data) intinfo siginfo))
                        (check-subprogram (verify-artifact arti record)
                                          (λ (result messages)
                                            (check-equal? result FAILURE)))
                        (call-with-snake-oil-cipher-trust
                         (λ ()
                           (check-subprogram (verify-artifact arti record)
                                             (λ (result messages)
                                               (check-pred void? result))))))))))))

  (test-case "Lock artifacts"
    (define with-content
      (artifact (text-source "qr")
                (integrity 'sha1 (text-source "st"))
                (signature (text-source "uv")
                           (text-source "wx"))))


    (define (try content?
                 integrity?
                 signature?
                 content-budget
                 digest-budget
                 public-key-budget
                 signature-budget
                 [arti with-content])
      (lock-artifact #:content? content?
                     #:integrity? integrity?
                     #:signature? signature?
                     #:content-budget content-budget
                     #:digest-budget digest-budget
                     #:public-key-budget public-key-budget
                     #:signature-budget signature-budget
                     arti
                     values))

    (check-match (try #t #t #t +inf.0 +inf.0 +inf.0 +inf.0)
                 (artifact #"qr"
                           (integrity 'sha1 #"st")
                           (signature #"uv" #"wx")))

    (check-match (try #f #t #t +inf.0 +inf.0 +inf.0 +inf.0)
                 (artifact (text-source "qr")
                           (integrity 'sha1 #"st")
                           (signature #"uv" #"wx")))

    (check-match (try #t #f #t +inf.0 +inf.0 +inf.0 +inf.0)
                 (artifact #"qr"
                           (integrity 'sha1 (text-source "st"))
                           (signature #"uv" #"wx")))

    (check-match (try #t #t #f +inf.0 +inf.0 +inf.0 +inf.0)
                 (artifact #"qr"
                           (integrity 'sha1 #"st")
                           (signature (text-source "uv")
                                      (text-source "wx"))))

    (check-match (try #t #t #t 0 +inf.0 +inf.0 +inf.0)
                 (artifact (text-source "qr")
                           (integrity 'sha1 #"st")
                           (signature #"uv" #"wx")))

    (check-match (try #t #t #t 0 0 +inf.0 +inf.0)
                 (artifact (text-source "qr")
                           (integrity 'sha1 (text-source "st"))
                           (signature #"uv" #"wx")))

    (check-match (try #t #t #t 0 0 0 +inf.0)
                 (artifact (text-source "qr")
                           (integrity 'sha1 (text-source "st"))
                           (signature (text-source "uv") #"wx")))

    (define (check-exhaust arti [expected 1])
      (check-equal? (lock-artifact arti values)
                    expected))

    (check-exhaust
     (artifact (exhausted-source 1)
               (integrity 'sha1 #"")
               (signature #"" #"")))

    (check-exhaust
     (artifact #""
               (integrity 'sha1 (exhausted-source 1))
               (signature #"" #"")))

    (check-exhaust
     (artifact #""
               (integrity 'sha1 #"")
               (signature (exhausted-source 1) #"")))

    (check-exhaust
     (artifact #""
               (integrity 'sha1 #"")
               (signature #"" (exhausted-source 1))))

    (check-exhaust
     (artifact (exhausted-source 1)
               (integrity 'sha1 (exhausted-source 2))
               (signature (exhausted-source 3)
                          (exhausted-source 4)))
     1)

    (check-exhaust
     (artifact #""
               (integrity 'sha1 (exhausted-source 2))
               (signature (exhausted-source 3)
                          (exhausted-source 4)))
     2)

    (check-exhaust
     (artifact #""
               (integrity 'sha1 #"")
               (signature (exhausted-source 3)
                          (exhausted-source 4)))
     3)

    (check-exhaust
     (artifact #""
               (integrity 'sha1 #"")
               (signature #""
                          (exhausted-source 4)))
     4)))
