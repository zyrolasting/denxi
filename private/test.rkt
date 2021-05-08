#lang racket/base

; Offers quick way to write tests using integrity and signature
; information.

(provide make-dummy-signature
         make-dummy-integrity
         call-with-dummy-trust)

(require racket/runtime-path)
(define-runtime-path private-key-path "privkey.pem")
(define-runtime-path dummy "dummy")
(define-runtime-path public-key-path "pubkey.pem")
(define-runtime-path password-path "pass")

(require "../codec.rkt"
         "../integrity.rkt"
         "../openssl.rkt"
         "../signature.rkt"
         "../source.rkt")


(define (make-dummy-signature digest)
  (signature-info
   (file-source (path->complete-path public-key-path))
   (make-signature-bytes digest
                         private-key-path
                         password-path)))


(define (make-dummy-integrity in [algo 'md5])
  (integrity-info algo
                  (make-digest in algo)))


(define (call-with-dummy-trust f)
  (XIDEN_TRUST_CHFS
   '(md5)
   (λ ()
     (XIDEN_TRUST_PUBLIC_KEYS
      (list (integrity 'md5 (make-digest public-key-path 'md5)))
      f))))

(module+ test
  (require rackunit)

  (define int (make-dummy-integrity dummy))
  (define sig (make-dummy-signature (integrity-info-digest int)))
  
  (call-with-dummy-trust
   (λ ()
     (check-pred $integrity-ok?
                 (check-integrity #:trust-bad-digest #f
                                  #:trust-message-digest-algorithms '(md5)
                                  int
                                  (integrity-info-digest int)))

     (check-pred $signature-ok?
                 (check-signature #:trust-public-key?
                                  (bind-trust-list (XIDEN_TRUST_PUBLIC_KEYS))
                                  #:public-key-path public-key-path
                                  #:trust-unsigned #f
                                  #:trust-bad-digest #f
                                  sig
                                  int)))))
