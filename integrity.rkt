#lang racket/base

; Verify integrity of bytes

(require "contract.rkt"
         "message.rkt"
         "openssl.rkt")

(provide (struct-out integrity-info)
         (contract-out
          [integrity
           (-> md-algorithm/c
               bytes?
               integrity-info?)]
          [well-formed-integrity-info/c
           flat-contract?]
          [bind-trust-list
           (-> (listof well-formed-integrity-info/c)
               (-> path-string? boolean?))]
          [check-integrity
           (-> #:trust-bad-digest any/c
               any/c
               md-bytes-source/c
               $integrity?)]))

(define+provide-message $integrity (ok? stage info))

(struct integrity-info (algorithm digest) #:prefab)

(define integrity integrity-info)

(define (digest-length-ok? info)
  (equal? (bytes-length (integrity-info-digest info))
          (bytes-length (make-digest #"whatever"
                                     (integrity-info-algorithm info)))))


(define well-formed-integrity-info/c
  (struct/c integrity-info
            md-algorithm/c
            bytes?))


(define (bind-trust-list trusted)
  (λ (public-key-path)
    (for/or ([integrity trusted])
      ($integrity-ok? (check-integrity #:trust-bad-digest #f integrity public-key-path)))))


;; -------------------------------------------------------------------------------
;; Affirmations

(define (consider-digest-trust #:trust-bad-digest trust-bad-digest intinfo k)
  (if trust-bad-digest
      (make-$integrity #t consider-digest-trust intinfo)
      (k)))


(define (consider-integrity-info #:trust-unknown-digest trust-unknown-digest intinfo k)
  (if (well-formed-integrity-info/c intinfo)
      (k)
      (make-$integrity trust-unknown-digest
                       consider-integrity-info
                       intinfo)))


(define (consider-digest-match intinfo variant)
  (make-$integrity (equal? (integrity-info-digest intinfo)
                           (make-digest variant (integrity-info-algorithm intinfo)))
                   consider-digest-match
                   intinfo))


(define (check-integrity #:trust-bad-digest trust-bad-digest intinfo variant)
  (consider-digest-trust #:trust-bad-digest trust-bad-digest intinfo
   (λ () (consider-integrity-info #:trust-unknown-digest trust-bad-digest intinfo
      (λ () (consider-digest-match intinfo variant))))))


(define (make-$integrity ok? p intinfo)
  ($integrity ok? (object-name p) intinfo))


(module+ test
  (require racket/function
           rackunit)

  ; Coverage info should flag this as uncovered when tests are passing.
  (define (fails . _)
    (fail "Control should not have reached here"))

  (define (make-dummy-integrity-info algorithm)
    (integrity-info algorithm
                    (make-digest (string->bytes/utf-8 (symbol->string algorithm))
                                 algorithm)))

  (test-case "Create integrity information"
    (for ([algorithm (in-list md-algorithms)])
      (define info (make-dummy-integrity-info algorithm))
      (check-pred integrity-info? info)
      (check-eq? (integrity-info-algorithm info) algorithm)
      (check-pred digest-length-ok? info)

      (test-equal? "Skip integrity checking"
                   (consider-digest-trust #:trust-bad-digest #t info fails)
                   ($integrity #t (object-name consider-digest-trust) info))

      (test-eq? "Proceed with integrity checking"
                (consider-digest-trust #:trust-bad-digest #f info (λ () 1))
                1)

      (test-case "Flag ill-formed integrity info as an unknown digest"
        (define (test-unknown v)
          (define (check trust?)
            (equal? (consider-integrity-info #:trust-unknown-digest trust? v fails)
                    ($integrity trust? (object-name consider-integrity-info) v)))
          (test-true (format "Count ~s as undeclared integrity info" v)
                     (and (check #t) (check #f))))

        (test-true (format "Count ~s as valid integrity info" info)
                   (consider-integrity-info #:trust-unknown-digest #f info (const #t)))

        (test-unknown 'garbage)
        (test-unknown (integrity-info 'garbage #"abc"))
        (test-unknown (integrity-info 'garbage "abc"))))))
