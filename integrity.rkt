#lang racket/base

; Define user-facing API for integrity subsystem.
;
; Warning: This module creates dependency cycles with lower level
; libraries.

(require racket/contract)
(provide
 (all-from-out "integrity/base.rkt")
 (contract-out
  [bind-trust-list
   (->* ((listof integrity?))
        ((listof chf?))
        (-> (or/c bytes? path-string? input-port?)
            boolean?))]
  [build-builtin-chf-trust
   (-> (listof symbol?)
       (listof chf?))]
  [call-with-snake-oil-chf-trust
   (-> (-> any) any)]
  [fetch-digest
   (-> well-formed-integrity?
       exhaust/c
       any/c)]
  [load-builtin-chf
   (-> symbol? (-> any) any)]
  [lock-integrity
   (->* (well-formed-integrity?)
        (#:digest-budget budget/c exhaust/c)
        well-formed-integrity?)]
  [make-sourced-digest
   (->* (source-variant? symbol?) (exhaust/c) bytes?)]
  [make-trusted-integrity
   (->* (source-variant?)
        (symbol?)
        raw-integrity?)]
  [make-user-chf-trust-predicate
   (-> (-> symbol? boolean?))]
  [malformed-integrity?
   flat-contract?]
  [snake-oil-chf
   chf?]
  [sourced-integrity?
   flat-contract?]
  [well-formed-integrity?
   flat-contract?]
  [DENXI_TRUST_BAD_DIGEST setting?]
  [DENXI_TRUST_CHFS setting?]))


;--------------------------------------------------------------------------------

(require (only-in racket/file file->bytes)
         (only-in racket/list index-of remove-duplicates)
         (only-in file/sha1 sha1-bytes)
         "crypto.rkt"
         "integrity/base.rkt"
         "integrity/ffi.rkt"
         "port.rkt"
         "setting.rkt"
         "source.rkt"
         "state.rkt")

(define-setting DENXI_TRUST_BAD_DIGEST boolean? #f)
(define-setting DENXI_TRUST_CHFS (listof symbol?) null)


(define sourced-integrity?
  (struct/c integrity symbol? source?))

(define well-formed-integrity?
  (or/c raw-integrity? sourced-integrity?))

(define malformed-integrity?
  (not/c well-formed-integrity?))


(define (build-builtin-chf-trust [trust-chfs (DENXI_TRUST_CHFS)])
  (chf-fold-trust
   load-builtin-chf
   (remove-duplicates trust-chfs)))


(define (make-user-chf-trust-predicate)
  (chf-bind-trust
   (append (current-chfs)
           (build-builtin-chf-trust (DENXI_TRUST_CHFS)))))


(define (load-builtin-chf denxi-sym [fail-thunk (λ () (raise ($chf-unavailable denxi-sym)))])
  (or (and (integrity-ffi-available?!)
           (integrity-ffi-chf-available?! denxi-sym)
           (λ (in) (integrity-ffi-make-digest! in denxi-sym)))
      (fail-thunk)))


(define (fetch-digest intinfo exhaust)
  (state-add (coerce-source (integrity-digest intinfo))
             (struct-copy transfer-policy zero-trust-transfer-policy
                          [max-size MAX_EXPECTED_DIGEST_LENGTH]
                          [buffer-size MAX_EXPECTED_DIGEST_LENGTH]
                          [timeout-ms (DENXI_FETCH_TIMEOUT_MS)]
                          [telemeter void])))


(define (make-sourced-digest variant algorithm [exhaust raise])
  (if (input-port? variant)
      (make-digest variant algorithm)
      (fetch (coerce-source variant)
             (λ (in est-size) (port->bytes in))
             exhaust)))


(define (make-trusted-integrity source [chf (get-default-chf)])
  (fetch (coerce-source source)
         (λ (in est-size) (integrity chf (make-digest in chf)))
         raise))


(define (lock-integrity intinfo
                        #:digest-budget [digest-budget MAX_EXPECTED_DIGEST_LENGTH]
                        [exhaust raise])
  (define locked
    (lock-source (integrity-digest intinfo)
                 digest-budget
                 exhaust))

  (if (eq? locked (integrity-digest intinfo))
      intinfo
      (integrity
       (integrity-chf-symbol intinfo)
       locked)))


(define (bind-trust-list claims [chf-trust (current-chfs)])
  (let ([trusted (map lock-integrity claims)]
        [trust? (chf-bind-trust chf-trust)])
    (λ (in)
      (for/or ([instance (in-list trusted)])
        (integrity-check-passed?
         (check-integrity
          #:trust-bad-digest #f
          trust?
          instance
          (make-digest (if (input-port? in)
                           (peeking-input-port in)
                           in)
                       (integrity-chf-symbol instance))))))))


(define snake-oil-chf
  (chf 'sha1
       #px"^(?i:sha-?1)$"
       sha1-bytes))


(define (call-with-snake-oil-chf-trust f)
  (parameterize ([current-chfs (list snake-oil-chf)]) (f)))


(module+ test
  (require racket/file
           racket/function
           rackunit
           "codec.rkt"
           "crypto.rkt"
           "source.rkt")


  (check-false (sourced-integrity? (integrity '|| #"")))
  (check-true (sourced-integrity? (integrity '|| (byte-source #""))))
  (define (check-symmetry v)
    (check-eq? (not (well-formed-integrity? v))
               (malformed-integrity? v)))

  (check-symmetry (integrity '|| #""))
  (check-symmetry (integrity #f #""))
  (check-symmetry (integrity '|| #f))

  (call-with-snake-oil-chf-trust
   (λ ()
     (define (make-dummy-integrity algorithm)
       (integrity algorithm
                       (make-digest (string->bytes/utf-8 (symbol->string algorithm))
                                    algorithm)))

     (define expected-digest #"\251\231>6G\6\201j\272>%qxP\302l\234\320\330\235")

     (test-case "Make trusted integrity info"
       (define val (make-trusted-integrity #"abc" 'sha1))
       (check-pred integrity? val)
       (check-equal? (integrity-chf-symbol val) 'sha1)
       (check-equal? (integrity-digest val) expected-digest))

     (test-equal? "Lock integrity info"
                  (lock-integrity (integrity 'sha1 (text-source "abc")))
                  (integrity 'sha1 #"abc"))

     (test-case "Do not lock integrity info if budget is too low"
       (check-match (lock-integrity #:digest-budget 0
                                    (integrity 'sha1 (text-source "abc")))
                    (integrity 'sha1 (text-source "abc"))))

     (test-equal? "Forward exhaust procedure"
                  (with-handlers ([values values])
                    (lock-integrity (integrity 'sha1 (exhausted-source 1))))
                  1)

     (test-case "Bind trust in data"
       (define trust?
         (bind-trust-list
          (list (integrity 'sha1 expected-digest))))
       (check-true (trust? (open-input-bytes #"abc")))
       (define (check-fail v)
         (check-false (trust? (open-input-bytes v))))
       (check-fail expected-digest)
       (check-fail #"")
       (check-fail #"a")
       (check-fail #"ab")
       (check-fail #"bc")
       (check-fail #"ac")))))
