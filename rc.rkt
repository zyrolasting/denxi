#lang racket/base

; Define runtime configuration.

(require racket/function
         racket/path
         racket/sandbox
         "codec.rkt"
         "contract.rkt"
         "file.rkt"
         "integrity.rkt"
         "openssl.rkt"
         "plugin.rkt"
         "setting.rkt"
         "string.rkt"
         "url.rkt"
         "workspace.rkt")

(provide
 (contract-out
  [XIDEN_SETTINGS
   (hash/c symbol? setting? #:immutable #t)]
  [dump-xiden-settings
   (-> (hash/c symbol? any/c))]))


; This gets build out as a side-effect of instantiation. If you build
; it declaratively, then it's easy to forget to add a new setting
; unless you expand into a complete hash table.
(define XIDEN_SETTINGS (hasheq))


; The only difference between a vanilla setting an a Xiden setting is how a
; fallback value is computed.
(define-syntax-rule (define-xiden-setting id cnt default-value)
  (begin (provide (contract-out [id setting?]))
         (define-setting id cnt (xiden-setting-find-value default-value))
         (set! XIDEN_SETTINGS (hash-set XIDEN_SETTINGS 'id id))))


; Return a procedure to fetch a value for a setting if it is not already set.
(define (xiden-setting-find-value default-value)
  (λ (id)
    (for/or ([make-value (in-xiden-setting-value-sources id default-value)])
      (define maybe-not-void (make-value))
      (and (not (void? maybe-not-void))
           maybe-not-void))))


; Checks environment variable, then plugin, then hard-coded value.
(define (in-xiden-setting-value-sources id default-value)
  (in-list (list (λ () (maybe-get-setting-value-from-envvar (symbol->string id)))
                 (λ () (plugin-ref id (void)))
                 (const default-value))))


(define (maybe-get-setting-value-from-envvar envname)
  (define env (getenv envname))
  (cond [(not env) (void)]
        [(string=? env "") (void)]
        [else (read (open-input-string env))]))


(define (dump-xiden-settings)
  (for/hash ([(k v) (in-hash XIDEN_SETTINGS)])
    (values k (v))))



;; Begin runtime configuration space
;; =================================

(define-xiden-setting XIDEN_ALLOW_ENV (listof (or/c bytes-environment-variable-name? string?)) null)
(define-xiden-setting XIDEN_ALLOW_UNSUPPORTED_RACKET boolean? #f)
(define-xiden-setting XIDEN_BYTE_ENCODING xiden-encoding/c 'base64)
(define-xiden-setting XIDEN_DOWNLOAD_MAX_REDIRECTS exact-nonnegative-integer? 2)
(define-xiden-setting XIDEN_FASL_OUTPUT boolean? #f)
(define-xiden-setting XIDEN_FETCH_BUFFER_SIZE_MB (real-in 0.1 20) 10)
(define-xiden-setting XIDEN_FETCH_PKGDEF_SIZE_MB (real-in 0.1 20) 0.1)
(define-xiden-setting XIDEN_FETCH_TIMEOUT_MS (real-in 100 (* 1000 10)) 3000)
(define-xiden-setting XIDEN_FETCH_TOTAL_SIZE_MB (or/c +inf.0 real?) 100)
(define-xiden-setting XIDEN_GENERATED_INPUT_NAME string? DEFAULT_STRING)
(define-xiden-setting XIDEN_TRUST_HOST_EXECUTABLES (listof string?) null)
(define-xiden-setting XIDEN_INPUT_OVERRIDES (listof (list/c (or/c symbol? string? regexp? pregexp? byte-regexp? byte-pregexp?) list?)) null)
(define-xiden-setting XIDEN_INSTALL_ABBREVIATED_SOURCES (listof string?) null)
(define-xiden-setting XIDEN_INSTALL_DEFAULT_SOURCES (listof (list/c string? string?)) null)
(define-xiden-setting XIDEN_INSTALL_SOURCES (listof (list/c string? string? string?)) null)
(define-xiden-setting XIDEN_MEMORY_LIMIT_MB (>=/c 0) 200)
(define-xiden-setting XIDEN_MESSAGE_DIGEST_ALGORITHM md-algorithm/c 'sha384)
(define-xiden-setting XIDEN_READER_FRIENDLY_OUTPUT boolean? #f)
(define-xiden-setting XIDEN_SIGNER (list/c (or/c #f string?) (or/c #f path-string?) (or/c #f path-string?)) '(#f #f #f))
(define-xiden-setting XIDEN_SUBPROCESS_TIMEOUT_S (>=/c 0) (* 30 60))
(define-xiden-setting XIDEN_TIME_LIMIT_S (>=/c 0) (* 5 60))
(define-xiden-setting XIDEN_TRUST_EXECUTABLES (listof well-formed-integrity-info/c) null)
(define-xiden-setting XIDEN_TRUST_PUBLIC_KEYS (listof well-formed-integrity-info/c) null)
(define-xiden-setting XIDEN_TRUST_MESSAGE_DIGEST_ALGORITHMS (listof md-algorithm/c) null)
(define-xiden-setting XIDEN_TRUST_ANY_EXECUTABLE boolean? #f)
(define-xiden-setting XIDEN_TRUST_ANY_PUBLIC_KEY boolean? #f)
(define-xiden-setting XIDEN_TRUST_BAD_DIGEST boolean? #f)
(define-xiden-setting XIDEN_TRUST_BAD_SIGNATURE boolean? #f)
(define-xiden-setting XIDEN_TRUST_UNSIGNED boolean? #f)
(define-xiden-setting XIDEN_TRUST_UNVERIFIED_HOST boolean? #f)
(define-xiden-setting XIDEN_VERBOSE boolean? #f)

(module+ test
  (require rackunit
           (submod "file.rkt" test)
           (submod "plugin.rkt" test))

  (test-not-exn "Group all runtime configuration in a hash table"
                (λ ()
                  (invariant-assertion
                   (and/c immutable? (hash/c symbol? setting?))
                   XIDEN_SETTINGS)))


  (test-case "Dump current runtime configuration to a hash table"
    (check-not-exn
     (λ ()
       (invariant-assertion
        (and/c immutable? (hash/c symbol? (not/c setting?)))
        (dump-xiden-settings))))

    (define dump (dump-xiden-settings))
    (check-equal? (hash-ref dump 'XIDEN_GENERATED_INPUT_NAME)
                  (XIDEN_GENERATED_INPUT_NAME))

    (XIDEN_GENERATED_INPUT_NAME
     "foo"
     (λ ()
       (check-not-equal? (hash-ref dump 'XIDEN_GENERATED_INPUT_NAME)
                         (XIDEN_GENERATED_INPUT_NAME))
       (check-equal? (hash-ref (dump-xiden-settings) 'XIDEN_GENERATED_INPUT_NAME)
                     (XIDEN_GENERATED_INPUT_NAME)))))

  (test-workspace "Find fallback values from several sources"
    (test-equal? "First use a hard-coded value"
                 (XIDEN_GENERATED_INPUT_NAME)
                 DEFAULT_STRING)

    (test-case "Override hard-coded value with rcfile"
      (call-with-plugin
       `(module rc racket/base
          (provide (all-defined-out))
          (define XIDEN_GENERATED_INPUT_NAME "foo"))
       (λ ()
         (check-equal? (XIDEN_GENERATED_INPUT_NAME) "foo")
         (test-case "Override rcfile value with envvar value"
           (dynamic-wind (λ () (putenv "XIDEN_GENERATED_INPUT_NAME" "\"bar\""))
                         (λ () (check-equal? (XIDEN_GENERATED_INPUT_NAME) "bar"))
                         (λ ()
                           (putenv "XIDEN_GENERATED_INPUT_NAME" "")
                           (test-equal? "Do not use empty envvar strings as a source for settings"
                                        (XIDEN_GENERATED_INPUT_NAME)
                                        "foo"))))))))

  (test-workspace "Lazily validate fallback values"
    (test-exn "Reject invalid values from ennvar"
              exn:fail:contract?
              (λ ()
                (dynamic-wind void
                              (λ ()
                                (putenv "XIDEN_GENERATED_INPUT_NAME" "123")
                                (XIDEN_GENERATED_INPUT_NAME))
                              (λ ()
                                (putenv "XIDEN_GENERATED_INPUT_NAME" "")))))))
