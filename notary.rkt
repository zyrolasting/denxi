#lang racket/base

(require racket/contract)
(provide (struct-out notary)
         (contract-out
          [lazy-notary notary?]
          [make-fraudulent-notary
           (-> symbol? notary?)]
          [make-notary
           (->* (string?)
                (#:private-key
                 (or/c #f source-variant?)
                 #:public-key-source
                 (or/c #f source-variant?)
                 #:private-key-password
                 (or/c #f source-variant?))
                notary?)]
          [notarize
           (-> notary?
               (or/c artifact? source-variant?)
               (machine/c artifact?))]))


(require racket/file
         racket/match
         "artifact.rkt"
         "crypto.rkt"
         "integrity.rkt"
         "machine.rkt"
         "signature.rkt"
         "source.rkt")


(struct notary
  (chf
   public-key-source
   private-key
   private-key-password))


(define (make-notary chf
                     #:public-key-source [pb #f]
                     #:private-key [pk #f]
                     #:private-key-password [pkp #f])
  (notary chf pb pk pkp))


(define lazy-notary
  (make-notary))


; Don't reduce to normal constructor call. This counts as implicit
; test coverage based on module instantiations.
(define (make-fraudulent-notary [chf (get-default-chf)])
  (make-notary #:chf chf
               #:public-key-source
               snake-oil-public-key
               #:private-key
               snake-oil-private-key
               #:private-key-password
               snake-oil-private-key-password))


(define (notarize the-notary source)
  (match-define (notary chf pubkey prvkey prvkeypass) the-notary)
  (if chf
      (machine-fetch source
       (λ (in est-size)
         (define intinfo
           (integrity chf (make-digest in chf)))
         (close-input-port in)         
         (artifact source
                   intinfo
                   (and pubkey
                        prvkey
                        (signature
                         pubkey
                         (make-signature
                          (integrity-digest intinfo)
                          (integrity-chf-symbol intinfo)
                          prvkey
                          prvkeypass))))))
      (machine-unit (artifact source #f #f))))


(module+ test
  (require rackunit
           (submod "machine.rkt" test))
  (check-pred notary? (make-notary))
  (call-with-snake-oil-cipher-trust
   (λ ()
     (check-machine-value
      (notarize (make-fraudulent-notary)
                (make-artifact #"abc"))
      (artifact #"abc"
                (integrity (? symbol? _)
                           (? bytes? _))
                (signature (? bytes? _)
                           (? bytes? _)))))))
