#lang racket/base

(require racket/contract)
(provide
 (contract-out
  [make-http-shovel
   (->* (url-variant?)
        ((or/c #f symbol?)
         (or/c #f source-variant?))
        shovel/c)]))

(require racket/format
         racket/list
         "../artifact.rkt"
         "../crypto.rkt"
         "../dig.rkt"
         "../integrity.rkt"
         "../subprogram.rkt"
         "../signature.rkt"
         "../source.rkt"
         "../url.rkt")


(define (make-http-shovel base-url-variant [chf #f] [public-key-source #f])
  (let ([base-url (coerce-url base-url-variant)])
    (λ (key)
      (if (url-variant? key)
          (let* ([ext-url (extend-base-url base-url (coerce-url key))])
            (subprogram-unit
             (artifact (http-source ext-url)
                       (and chf
                            (integrity-info chf
                                            (http-source
                                             (add-url-extname ext-url (~a "." chf)))))
                       (and public-key-source
                            (signature-info public-key-source
                                            (http-source
                                             (add-url-extname ext-url (~a "." chf ".sig"))))))))
          (dig-failure 'http-shovel key)))))

(define (add-url-extname ext-url extname)
  (struct-copy url ext-url
               [path
                (let ([pps (url-path ext-url)])
                  (if (null? pps)
                      (list (path/param extname null))
                      (let* ([last-index (sub1 (length pps))]
                             [last-pp (list-ref pps last-index)])
                        (list-set pps
                                  last-index
                                  (struct-copy path/param last-pp
                                               [path (~a (path/param-path last-pp)
                                                         extname)])))))]))

(define (extend-base-url base-url ext-url)
  (struct-copy url base-url
               [path
                (append (url-path base-url)
                        (url-path ext-url))]
               [query
                (append (url-query base-url)
                        (url-query ext-url))]))

(module+ test
  (require rackunit)

  (define (check-source src expected)
    (check-pred http-source? src)
    (check-equal? (url->string (http-source-request-url src))
                  expected))
  
  (test-case "Create artifacts expecting only content"
    (define dig (make-http-shovel "https://example.com/pkg"))
    (define arti (get-subprogram-value (dig "/cool/stuff?a=b")))
    (check-pred artifact? arti)
    (check-false (artifact-integrity arti))
    (check-false (artifact-signature arti))
    (check-source (artifact-source arti)
                  "https://example.com/pkg/cool/stuff?a=b"))

  (test-case "Create artifacts expecting content and integrity info"
    (define dig (make-http-shovel "https://example.com/pkg" 'md5))
    (define arti (get-subprogram-value (dig "x")))
    (check-pred artifact? arti)
    (check-pred integrity-info? (artifact-integrity arti))
    (check-false (artifact-signature arti))

    (check-source (artifact-source arti)
                  "https://example.com/pkg/x")
    (check-source (integrity-info-digest (artifact-integrity arti))
                  "https://example.com/pkg/x.md5"))

  (test-case "Create artifacts expecting all info"
    (define dig (make-http-shovel "https://example.com/pkg" 'md5 empty-source))
    (define arti (get-subprogram-value (dig "x")))
    (check-pred artifact? arti)
    (check-pred integrity-info? (artifact-integrity arti))
    (check-pred signature-info? (artifact-signature arti))

    (check-source (artifact-source arti)
                  "https://example.com/pkg/x")
    (check-source (integrity-info-digest (artifact-integrity arti))
                  "https://example.com/pkg/x.md5")

    (check-eq? (signature-info-pubkey (artifact-signature arti))
               empty-source)

    (check-source (signature-info-body (artifact-signature arti))
                  "https://example.com/pkg/x.md5.sig")))

