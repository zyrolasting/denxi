#lang racket/base

; Verify integrity of bytes

(require "contract.rkt"
         "file.rkt"
         "format.rkt"
         "localstate.rkt"
         "message.rkt"
         "openssl.rkt"
         "port.rkt"
         "source.rkt"
         "strict-rc.rkt"
         "workspace.rkt")

(provide (struct-out integrity-info)
         (contract-out
          [integrity
           (-> md-algorithm/c
               source-variant?
               integrity-info?)]
          [make-sourced-digest
           (->* (source-variant?
                 md-algorithm/c)
                (exhaust/c)
                bytes?)]
          [well-formed-integrity-info/c
           flat-contract?]
          [bind-trust-list
           (-> (listof well-formed-integrity-info/c)
               (-> path-string? boolean?))]
          [check-integrity
           (-> #:trust-bad-digest any/c
               any/c
               source-variant?
               $integrity?)]))

(define+provide-message $integrity (ok? stage info))

(struct integrity-info (algorithm digest) #:prefab)

(define integrity integrity-info)

(define MAX_EXPECTED_DIGEST_LENGTH 128)

(define (digest-length-ok? info)
  (equal? (bytes-length (integrity-info-digest info))
          (bytes-length (make-digest #"whatever"
                                     (integrity-info-algorithm info)))))

(define well-formed-integrity-info/c
  (struct/c integrity-info
            md-algorithm/c
            source-variant?))


(define (bind-trust-list trusted)
  (λ (path)
    (for/or ([integrity trusted])
      ($integrity-ok? (check-integrity #:trust-bad-digest #f integrity path)))))


(define (fetch-digest intinfo exhaust)
  (let ([source (coerce-source (integrity-info-digest intinfo))])
    (fetch source
           (λ (in est-size)
             (file->bytes
              (build-workspace-path
               (path-record-path
                (make-addressable-file
                 #:cache-key (make-source-key source)
                 #:max-size MAX_EXPECTED_DIGEST_LENGTH
                 #:buffer-size MAX_EXPECTED_DIGEST_LENGTH
                 #:timeout-ms (rc-ref 'XIDEN_FETCH_TIMEOUT_MS)
                 #:on-status void
                 "_"
                 in
                 est-size)))))
           exhaust)))


(define (make-sourced-digest variant algorithm [exhaust raise])
  (if (input-port? variant)
      (make-digest variant algorithm)
      (fetch (coerce-source variant)
             (λ (in est-size) (make-digest in algorithm))
             exhaust)))


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
  (call/cc
   (λ (abort)
     (define (return v)
       (make-$integrity v consider-digest-match intinfo))
     (return (equal? (fetch-digest intinfo (λ _ (return #f)))
                     (make-sourced-digest variant (integrity-info-algorithm intinfo)))))))


(define (check-integrity #:trust-bad-digest trust-bad-digest intinfo variant)
  (consider-digest-trust #:trust-bad-digest trust-bad-digest intinfo
   (λ () (consider-integrity-info #:trust-unknown-digest trust-bad-digest intinfo
      (λ () (consider-digest-match intinfo variant))))))


(define (make-$integrity ok? p intinfo)
  ($integrity ok? (object-name p) intinfo))


(module+ test
  (require racket/function
           racket/runtime-path
           rackunit)

  ; Coverage info should flag this as uncovered when tests are passing.
  (define (fails . _)
    (fail "Control should not have reached here"))

  (define (make-dummy-integrity-info algorithm)
    (integrity-info algorithm
                    (make-digest (string->bytes/utf-8 (symbol->string algorithm))
                                 algorithm)))

  (define-runtime-path integrity.rkt "integrity.rkt")
  (define-runtime-path main.rkt "main.rkt")

  (test-case "Bind trust in specific paths"
    (define trust?
      (bind-trust-list
       (list (integrity-info 'sha1 (make-digest integrity.rkt 'sha1)))))
    (check-true (trust? integrity.rkt))
    (check-false (trust? main.rkt)))

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
