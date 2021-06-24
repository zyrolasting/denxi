#lang racket/base

; User-facing API for integrity subsystem

(require racket/contract)
(provide
 (struct-out integrity-info/sourced)
 (all-from-out "integrity/base.rkt"
               "integrity/chf.rkt")
 (contract-out
  [XIDEN_TRUST_BAD_DIGEST setting?]
  [XIDEN_TRUST_CHFS setting?]
  [integrity
   (-> symbol?
       source-variant?
       integrity-info/sourced?)]
  [make-sourced-digest
   (->* (source-variant? symbol?) (exhaust/c) bytes?)]
  [fetch-digest
   (-> well-formed-integrity-info/c exhaust/c any/c)]
  [make-trusted-integrity-info
   (->* (source-variant?)
        (symbol?)
        integrity-info?)]
  [lock-integrity-info
   (->* ((or/c integrity-info/sourced? well-formed-integrity-info/c))
        (#:digest-budget budget/c exhaust/c)
        well-formed-integrity-info/c)]
  [load-builtin-chf
   (-> symbol? (-> any) any)]
  [bind-trust-list
   (-> (listof (or/c integrity-info/sourced? integrity-info?))
       (-> (or/c bytes? path-string? input-port?)
           boolean?))]))

(require (only-in racket/file file->bytes)
         (only-in racket/list index-of)
         "crypto.rkt"
         "integrity/base.rkt"
         "integrity/chf.rkt"
         "integrity/fallback.rkt"
         "integrity/ffi.rkt"
         "port.rkt"
         "setting.rkt"
         "source.rkt"
         "state.rkt")


; High-level integrity claim that may contain a source.
(struct integrity-info/sourced (chf digest-source))
(define integrity integrity-info/sourced)


(define-setting XIDEN_TRUST_BAD_DIGEST boolean? #f)
(define-setting XIDEN_TRUST_CHFS (listof symbol?) null)


(define (load-builtin-chf xiden-sym [fail-thunk (λ () (raise ($chf-unavailable xiden-sym)))])
  (or (and (integrity-ffi-available?!)
           (integrity-ffi-chf-available?! xiden-sym)
           (λ (in _) (integrity-ffi-make-digest! in xiden-sym)))
      (and (fallback-chf-available? xiden-sym)
           (λ (in _) (fallback-make-digest in xiden-sym)))
      (fail-thunk)))


(define (call-with-simplified-chf-trust #:trust trust-chfs #:wrt wrt f)
  (parameterize ([current-chfs
                  (foldl
                   (λ (trusted-chf wip)
                     (cons (cons (list trusted-chf)
                                 (load-builtin-chf trusted-chf))
                           wip))
                   wrt
                   trust-chfs)])
    (f)))


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
                 #:timeout-ms (XIDEN_FETCH_TIMEOUT_MS)
                 #:on-status void
                 "_"
                 in
                 est-size)))))
           exhaust)))


(define (make-sourced-digest variant algorithm [exhaust raise])
  (if (input-port? variant)
      (make-digest variant algorithm)
      (fetch (coerce-source variant)
             (λ (in est-size) (port->bytes in))
             exhaust)))


(define (make-trusted-integrity-info source [chf (get-default-chf)])
  (fetch (coerce-source source)
         (λ (in est-size) (integrity-info chf (make-digest in chf)))
         raise))


(define (lock-integrity-info intinfo
                             #:digest-budget [digest-budget MAX_EXPECTED_DIGEST_LENGTH]
                             [exhaust raise])
  (if (integrity-info? intinfo)
      intinfo
      (integrity-info
       (find-chf-canonical-name (integrity-info/sourced-chf intinfo)
                                (current-chfs))
       (lock-source (integrity-info/sourced-digest-source intinfo)
                    digest-budget
                    exhaust))))



(define (bind-trust-list claims)
  (let ([trusted (map lock-integrity-info claims)])
    (λ (in)
      (for/or ([integrity trusted])
        ($integrity-ok? (check-integrity
                         #:trust-bad-digest #f
                         #:trust-message-digest-algorithms (XIDEN_TRUST_CHFS)
                         integrity
                         (make-digest in
                                      (integrity-info-algorithm integrity))))))))

(module+ test
  (require racket/file
           racket/function
           rackunit
           (submod "integrity/chf.rkt" test)
           "codec.rkt"
           "crypto.rkt"
           "source.rkt")

  (define (make-dummy-integrity-info algorithm)
    (integrity-info algorithm
                    (make-digest (string->bytes/utf-8 (symbol->string algorithm))
                                 algorithm)))

  (test-case "Make trusted integrity info"
    (define val (make-trusted-integrity-info #"abc" 'md5))
    (check-pred integrity-info? val)
    (check-equal? (integrity-info-algorithm val) 'md5)
    (check-equal? (integrity-info-digest val)
                  #"\220\1P\230<\322O\260\326\226?}(\341\177r"))

  (test-case "Lock integrity info"
    (check-match (lock-integrity-info (integrity 'md5 (text-source "abc")))
                 (integrity-info 'md5 #"abc")))

  (test-case "Do not lock integrity info if budget is too low"
    (check-match (lock-integrity-info #:digest-budget 0
                                      (integrity 'md5 (text-source "abc")))
                 (integrity-info 'md5 (text-source "abc"))))

  (test-equal? "Forward exhaust procedure"
               (with-handlers ([real? values])
                 (lock-integrity-info (integrity 'md5 (exhausted-source 1))))
               1)

  (test-case "Bind trust in data"
    (define trust?
      (bind-trust-list
       (list (integrity-info 'sha1 (make-digest test-digest-data 'sha1)))))
    (XIDEN_TRUST_CHFS '(sha1)
      (λ ()
        (check-true (trust? (open-input-bytes test-digest-data)))))))
