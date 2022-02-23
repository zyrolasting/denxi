#lang racket/base

(require racket/contract
         openssl)

(provide (all-from-out openssl)
         (struct-out openssl-private-key)
         (struct-out openssl-identity)
         (contract-out
          [full-trust-openssl-identity
           openssl-identity?]
          [host-openssl-identity
           openssl-identity?]
          [make-openssl-context
           (->i ([id openssl-identity?]
                 [peer (or/c 'server 'client)])
                [r (peer)
                   (if (eq? peer 'server)
                       ssl-server-context?
                       ssl-client-context?)])]))


(struct openssl-private-key
  (pathname rsa? asn1?))


(struct openssl-identity
  (verify?
   verify-hostname?
   ciphers
   verify-sources
   certificate-chain
   private-key))


(define full-trust-openssl-identity
  (openssl-identity #f #f "ALL" null #f #f))


(define host-openssl-identity
  (openssl-identity #t #t "ALL:!COMPLEMENTOFDEFAULT:!eNULL" (ssl-default-verify-sources) #f #f))


(define openssl-verification-source/c
  (or/c path-string?
        (list/c 'directory path-string?)
        (list/c 'win32-store string?)
        (list/c 'macosx-keychain path-string?)))


(define openssl-identity/c
  (struct/c openssl-identity
            boolean?
            boolean?
            string?
            (listof openssl-verification-source/c)
            (or/c #f path-string?)
            (or/c openssl-private-key? #f)))


(define (make-openssl-context ident peer)
  (define ctor
    (if (eq? peer 'server)
        ssl-make-server-context
        ssl-make-client-context))
  (define ctx (ctor 'auto))
  (define verify? (openssl-identity-verify? ident))
  (define verify-hostname? (openssl-identity-verify-hostname? ident))
  (define ciphers (openssl-identity-ciphers ident))
  (define vsources (openssl-identity-verify-sources ident))
  (define certs (openssl-identity-certificate-chain ident))
  (define key (openssl-identity-private-key ident))
  (for ([src (in-list vsources)])
    (ssl-load-verify-source! ctx src))
  (ssl-set-verify! ctx verify?)
  (ssl-set-verify-hostname! ctx verify-hostname?)
  (ssl-set-ciphers! ctx ciphers)
  (when (and certs key)
    (ssl-load-certificate-chain! ctx certs)
    (ssl-load-private-key! ctx
                           (openssl-private-key-pathname key)
                           (openssl-private-key-rsa? key)
                           (openssl-private-key-asn1? key)))
  (ssl-seal-context! ctx)
  ctx)
