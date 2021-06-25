#lang racket/base

(require racket/contract)

(provide (struct-out notary)
         (contract-out
          [lazy-notary notary?]
          [make-fraudulent-notary
           (-> symbol? notary?)]
          [make-notary
           (->* ()
                (#:chf symbol?
                 #:private-key
                 (or/c #f source-variant?)
                 #:public-key-source
                 (or/c #f source-variant?)
                 #:private-key-password
                 (or/c #f source-variant?))
                notary?)]
          [notarize
           (-> notary?
               (or/c artifact? source-variant?)
               (subprogram/c artifact?))]))

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

(define (make-notary #:chf [chf (get-default-chf)]
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


(define (notarize the-notary content)
  (match-define (notary chf pubkey prvkey prvkeypass) the-notary)

  (define user-source
    (if (source-variant? content)
        content
        (artifact-source content)))

  (define content-source
    (coerce-source user-source))

  (if chf
      (subprogram-fetch
       "notary"
       content-source
       (λ (in est-size)
         (define intinfo
           (and chf
                (integrity
                 chf
                 (make-digest in chf))))

         (close-input-port in)
         
         (artifact user-source
                   intinfo
                   (and intinfo
                        pubkey
                        prvkey
                        (signature
                         pubkey
                         (make-signature
                          (integrity-digest intinfo)
                          (integrity-chf-symbol intinfo)
                          prvkey
                          prvkeypass))))))
      (subprogram-unit (artifact user-source #f #f))))

(module+ test
  (require rackunit
           (submod "subprogram.rkt" test))
  (check-pred notary? (make-notary))
  (check-match
   (get-subprogram-value
    (notarize (make-fraudulent-notary)
              (artifact #"abc")))
   (artifact (not #f)
             (integrity (? symbol? _)
                        (? bytes? _))
             (signature (? bytes? _)
                        (? bytes? _)))))
