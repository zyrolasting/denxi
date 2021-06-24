#lang racket/base

; User-facing API for integrity subsystem

(require racket/contract)
(provide
 (struct-out integrity-info/sourced)
 (all-from-out "integrity/base.rkt"
               "integrity/chf.rkt")
 (contract-out
  [bind-trust-list
   (->* ((listof (or/c integrity-info/sourced? integrity-info?)))
        ((or/c #f (listof symbol?)))
        (-> (or/c bytes? path-string? input-port?)
            boolean?))]
  [fetch-digest
   (-> well-formed-integrity-info/c exhaust/c any/c)]
  [integrity
   (-> symbol?
       source-variant?
       integrity-info/sourced?)]
  [load-builtin-chf
   (-> symbol? (-> any) any)]
  [lock-integrity-info
   (->* ((or/c integrity-info/sourced? well-formed-integrity-info/c))
        (#:digest-budget budget/c exhaust/c)
        well-formed-integrity-info/c)]
  [make-sourced-digest
   (->* (source-variant? symbol?) (exhaust/c) bytes?)]
  [make-trusted-integrity-info
   (->* (source-variant?)
        (symbol?)
        integrity-info?)]
  [XIDEN_TRUST_BAD_DIGEST setting?]
  [XIDEN_TRUST_CHFS setting?]))


(require (only-in racket/file file->bytes)
         (only-in racket/list index-of remove-duplicates)
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

(define (build-builtin-chf-trust [trust-chfs (XIDEN_TRUST_CHFS)])
  (chf-fold-trust
   load-builtin-chf
   (remove-duplicates trust-chfs)))


(define (load-builtin-chf xiden-sym [fail-thunk (λ () (raise ($chf-unavailable xiden-sym)))])
  (or (and (integrity-ffi-available?!)
           (integrity-ffi-chf-available?! xiden-sym)
           (λ (in _) (integrity-ffi-make-digest! in xiden-sym)))
      (and (fallback-chf-available? xiden-sym)
           (λ (in _) (fallback-make-digest in xiden-sym)))
      (fail-thunk)))


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



(define (bind-trust-list claims [chf-trust (current-chfs)])
  (let ([trusted (map lock-integrity-info claims)]
        [trust? (chf-bind-trust chf-trust)])
    (λ (in)
      (for/or ([integrity (in-list trusted)])
        ($integrity-ok?
         (check-integrity
          #:trust-bad-digest #f
          trust?
          integrity
          (make-digest #:expect (integrity-info-digest integrity)
                       in
                       (integrity-info-algorithm integrity))))))))


(define snake-oil-chf-profile
  `([snake-oil] ,(λ (in _) (fallback-make-digest in 'sha1))))


(define (call-with-snake-oil-chf-profile f)
  (parameterize ([current-chfs (list snake-oil-chf-profile)]) (f)))


(module+ test
  (require racket/file
           racket/function
           rackunit
           "codec.rkt"
           "crypto.rkt"
           "source.rkt")

  (call-with-snake-oil-chf-profile
   (λ ()
     (define (make-dummy-integrity-info algorithm)
       (integrity-info algorithm
                       (make-digest (string->bytes/utf-8 (symbol->string algorithm))
                                    algorithm)))

     (define expected-digest #"\251\231>6G\6\201j\272>%qxP\302l\234\320\330\235")

     (test-case "Make trusted integrity info"
       (define val (make-trusted-integrity-info #"abc" 'snake-oil))
       (check-pred integrity-info? val)
       (check-equal? (integrity-info-algorithm val) 'snake-oil)
       (check-equal? (integrity-info-digest val) expected-digest))

     (test-equal? "Lock integrity info"
                  (lock-integrity-info (integrity 'snake-oil (text-source "abc")))
                  (integrity-info 'snake-oil #"abc"))

     (test-case "Do not lock integrity info if budget is too low"
       (check-match (lock-integrity-info #:digest-budget 0
                                         (integrity-info 'snake-oil (text-source "abc")))
                    (integrity-info 'snake-oil (text-source "abc"))))

     (test-equal? "Forward exhaust procedure"
                  (with-handlers ([real? values])
                    (lock-integrity-info (integrity 'snake-oil (exhausted-source 1))))
                  1)

     (test-case "Bind trust in data"
       (define trust?
         (bind-trust-list
          (list (integrity-info 'snake-oil expected-digest))))
       (check-true  (trust? (open-input-bytes #"abc")))
       (define (check-fail v)
         (check-false (trust? (open-input-bytes v))))
       (check-fail expected-digest)
       (check-fail #"")
       (check-fail #"a")
       (check-fail #"ab")
       (check-fail #"bc")
       (check-fail #"ac")))))
