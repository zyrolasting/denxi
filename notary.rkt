#lang racket/base

(require racket/contract)

(provide (struct-out notary)
         (contract-out
          [make-notary
           (->* ()
                (#:chf chf/c
                 #:private-key-path
                 (or/c #f path-string?)
                 #:public-key-source
                 (or/c #f source-variant?)
                 #:private-key-password-path
                 (or/c #f path-string?))
                notary?)]
          [notarize
           (-> notary?
               (or/c artifact-info? source-variant?)
               (subprogram/c artifact-info?))]
          [lazy-notary notary?]
          [fraudulent-notary notary?]))

(require racket/match
         "artifact.rkt"
         "crypto.rkt"
         "integrity.rkt"
         "signature.rkt"
         "source.rkt"
         "subprogram.rkt"
         "private/test.rkt")

(struct notary
  (chf
   public-key-source
   private-key-path
   private-key-password-path))

(define (make-notary #:chf [chf DEFAULT_CHF]
                     #:public-key-source [pb #f]
                     #:private-key-path [pk #f]
                     #:private-key-password-path [pkp #f])
  (notary chf pb pk pkp))


(define lazy-notary
  (make-notary))

; Don't reduce to normal constructor call. This counts as implicit
; test coverage based on module instantiations.
(define fraudulent-notary
  (make-notary #:chf DEFAULT_CHF
               #:public-key-source
               useless-public-key-path
               #:private-key-path
               leaked-private-key-path
               #:private-key-password-path
               leaked-private-key-password-path))


(define (notarize the-notary content)
  (match-define (notary chf pubkey prvkey prvkeypass) the-notary)

  (define user-source
    (if (source-variant? content)
        content
        (artifact-info-source content)))

  (define content-source
    (coerce-source user-source))

  (if chf
      (subprogram-fetch
       "notary"
       content-source
       (λ (in est-size)
         (define intinfo
           (and chf
                (integrity-info
                 chf
                 (make-digest in chf))))

         (close-input-port in)
         
         (artifact user-source
                   intinfo
                   (and intinfo
                        pubkey
                        prvkey
                        (signature-info
                         pubkey
                         (make-signature-bytes
                          (integrity-info-digest intinfo)
                          prvkey
                          prvkeypass))))))
      (subprogram-unit (artifact-info user-source #f #f))))

(module+ test
  (require rackunit
           (submod "subprogram.rkt" test))

  (check-pred notary? (make-notary))

  (check-match
    (get-subprogram-value
     (notarize fraudulent-notary
               (artifact #"abc")))
    (artifact-info (not #f)
                   (integrity-info (? chf/c _)
                                   (? bytes? _))
                   (signature-info (? path-string? _)
                                   (? bytes? _)))))
