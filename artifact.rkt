#lang racket/base

(require racket/contract
         "integrity.rkt"
         "signature.rkt")

(provide
 (struct-out artifact)
 (struct-out $artifact)
 (struct-out $artifact:signature)
 (struct-out $artifact:integrity)
 (contract-out
  [verify-artifact
   (-> artifact?
       known-implementation/c
       (subprogram/c void?))]
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
         "known.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt"
         "printer.rkt"
         "source.rkt"
         "subprogram.rkt")


(define-message $artifact ())
(define-message $artifact:integrity (status chf-symbol))
(define-message $artifact:signature (status public-key))

(struct artifact
  (source      ; Defines where bytes come from
   integrity   ; Integrity information: Did I get the right bytes?
   signature)) ; Signature for authentication: Did the bytes come from someone I trust?


(define (make-artifact source [integrity #f] [signature #f])
  (artifact source integrity signature))


(define (verify-artifact arti known)
  (mdo (check-artifact-integrity arti known)
       (check-artifact-signature arti known)
       (subprogram-unit (void))))


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


(define (check-artifact-integrity arti known)
  (subprogram
   (λ (messages)
     (match-define (artifact content int sig) arti)

     (define-values (int/use chf)
       (if (well-formed-integrity? int)
           (values (lock-integrity int)
                   (integrity-chf-symbol int))
           (values #f #f)))

     (define-values (from-bytes messages*)
       (run-subprogram (known-open-bytes known) messages))

     (define status
       (check-integrity
        #:trust-bad-digest trust-bad-digest
        (make-user-chf-trust-predicate)
        int/use
        (and chf
             (make-digest from-bytes (integrity-chf-symbol (artifact-integrity arti))))))

     (values (if (integrity-check-passed? status)
                 (void)
                 FAILURE)
             (cons ($artifact:integrity status chf)
                   messages*)))))



   (define-subprogram (check-artifact-signature arti path)
     (match-define (artifact content int sig) arti)
     (define sig/use
       (and (well-formed-signature? sig)
            (lock-signature sig)))
     (define int/use
       (and (well-formed-integrity? int)
            (lock-integrity int)))

     (define status
       (check-signature #:trust-unsigned trust-unsigned
                        #:trust-bad-digest trust-bad-digest
                        #:trust-public-key? trust-public-key?
                        #:verify-signature verify-signature
                        sig/use
                        int/use))
     ($attach (or (signature-check-passed? status) FAILURE)
              ($artifact:signature status
                                   (and (signature? sig/use)
                                        (signature-public-key sig/use)))))


(module+ test
  (require rackunit
           racket/file
           (submod "subprogram.rkt" test))


  (define-syntax-rule (trust-chf . x)
    (call-with-snake-oil-chf-trust (λ () . x)))

  (define-syntax-rule (trust-cipher . x)
    (call-with-snake-oil-cipher-trust (λ () . x)))

  (test-case "Verify artifacts"
    (trust-chf
     (define data
       #"abc")

     (define known
       (know))

     (run-subprogram (known-put-bytes known (open-input-bytes data)))

     (define intinfo
       (make-trusted-integrity data))

     (define siginfo
       (make-snake-oil-signature
        (integrity-digest intinfo)
        (integrity-chf-symbol intinfo)))

     (define arti
       (artifact (byte-source data) intinfo siginfo))

     (check-subprogram (verify-artifact arti known)
                       (λ (result messages)
                         (check-equal? result FAILURE)))

     (trust-cipher (check-subprogram (verify-artifact arti known)
                                     (λ (result messages)
                                       (check-pred void? result))))))

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
