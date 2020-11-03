#lang racket/base

; Verify integrity of bytes

(require "contract.rkt")

; Mirror OpenSSL
(define md-algorithms
  '(md4
    md5
    sha1
    sha224
    sha256
    sha3-224
    sha3-256
    sha3-384
    sha3-512
    sha384
    sha512
    sha512-224
    sha512-256))

(define md-bytes-source/c
  (or/c path-string? bytes? input-port?))

(define md-algorithm/c
  (apply or/c md-algorithms))

(provide (struct-out integrity-info)
         (contract-out
          [integrity
           (-> md-algorithm/c
               bytes?
               integrity-info?)]
          [md-algorithms
           (non-empty-listof symbol?)]
          [md-algorithm/c
           flat-contract?]
          [md-bytes-source/c
           flat-contract?]
          [well-formed-integrity-info/c
           flat-contract?]
          [make-digest
           (-> md-bytes-source/c
               md-algorithm/c
               bytes?)]
          [check-integrity
           (-> #:trust-bad-digest any/c
               any/c
               md-bytes-source/c
               $integrity?)]))

(require racket/sequence
         racket/format
         "codec.rkt"
         "file.rkt"
         "message.rkt"
         "openssl.rkt")

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


(define (make-digest variant algorithm)
  (cond [(path-string? variant)
         (call-with-input-file variant (λ (i) (make-digest i algorithm)))]
        [(bytes? variant)
         (make-digest (open-input-bytes variant) algorithm)]
        [(input-port? variant)
         (run-openssl-command variant
                              "dgst"
                              "-binary"
                              (~a "-" algorithm))]
        [else (raise-argument-error 'make-digest
                                    "A path, bytes, or an input port"
                                    variant)]))


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
