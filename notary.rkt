#lang racket/base

(require racket/contract)

(provide (struct-out notary)
         (contract-out
          [make-notary
           (->* ()
                (#:chf chf/c
                 #:private-key
                 (or/c #f source-variant?)
                 #:public-key-source
                 (or/c #f source-variant?)
                 #:private-key-password
                 (or/c #f source-variant?))
                notary?)]
          [notarize
           (-> notary?
               (or/c artifact-info? source-variant?)
               (subprogram/c artifact-info?))]
          [lazy-notary notary?]
          [fraudulent-notary notary?]))

(require racket/file
         racket/match
         "artifact.rkt"
         "crypto.rkt"
         "integrity.rkt"
         "signature.rkt"
         "source.rkt"
         "subprogram.rkt")

(struct notary
  (chf
   public-key-source
   private-key
   private-key-password))

(define (make-notary #:chf [chf DEFAULT_CHF]
                     #:public-key-source [pb #f]
                     #:private-key [pk #f]
                     #:private-key-password [pkp #f])
  (notary chf pb pk pkp))


(define lazy-notary
  (make-notary))

; Don't reduce to normal constructor call. This counts as implicit
; test coverage based on module instantiations.
(define fraudulent-notary
  (make-notary #:chf DEFAULT_CHF
               #:public-key-source
               snake-oil-public-key
               #:private-key
               snake-oil-private-key
               #:private-key-password
               snake-oil-private-key-password))


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
                         (make-signature
                          (integrity-info-digest intinfo)
                          (integrity-info-algorithm intinfo)
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
