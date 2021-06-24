#lang racket/base

; Private FFI accessors re: integrity checking

(require racket/contract
         (only-in racket/list index-of)
         (rename-in ffi/unsafe [-> -->])
         "chf.rkt"
         "../crypto.rkt")

(provide
 (contract-out
  [integrity-ffi-available?! (-> boolean?)]
  [integrity-ffi-chf-available?! predicate/c]
  [integrity-ffi-find-default-chf! (-> (or/c #f exact-nonnegative-integer?))]
  [integrity-ffi-get-c-chfs! (-> (or/c #f (listof symbol?)))]
  [integrity-ffi-get-chf-count! (-> any/c)]
  [integrity-ffi-get-default-chf-index! (-> any/c)]
  [integrity-ffi-get-get-digest-size! (-> any/c)]
  [integrity-ffi-get-load-chf! (-> any/c)]
  [integrity-ffi-get-make-digest! (-> any/c)]
  [integrity-ffi-get-supported-chfs! (-> any/c)]
  [integrity-ffi-load-chf! (-> symbol? any/c)]
  [integrity-ffi-make-digest! (-> input-port? symbol? bytes?)]))


(define _EVP_MD-pointer _pointer)

(define private-table (make-hash))


(define-syntax-rule (define-ffi-accessor id expr)
  (begin (define (id) expr)
         (hash-set! private-table 'id id)))


(define-syntax-rule (define-ffi-procedure sig . body)
  (define sig (assert-crypto-availability) . body))


(define (integrity-ffi-available?!)
  (and (for/and ([(k v) (in-hash private-table)]) v)
       #t))


(define (integrity-ffi-chf-available?! sym)
  (and (symbol? sym)
       (integrity-ffi-available?!)
       (member sym (integrity-ffi-get-c-chfs!))
       #t))

(define-ffi-accessor integrity-ffi-get-default-chf-index!
  (crypto-get-obj! #"XIDEN_DEFAULT_CHF_INDEX" _uint))


(define-ffi-accessor integrity-ffi-get-load-chf!
  (crypto-get-obj! #"xiden_load_chf"
                   (_fun _uint --> _EVP_MD-pointer)))


(define-ffi-accessor integrity-ffi-get-get-digest-size!
  (crypto-get-obj! #"xiden_get_digest_size"
                   (_fun _EVP_MD-pointer --> _int)))


(define-ffi-accessor integrity-ffi-get-make-digest!
  (crypto-get-obj! #"xiden_make_digest"
                   (_fun _EVP_MD-pointer
                         _bytes
                         (_fun (_cpointer _int)
                               _uint
                               --> _gcpointer)
                         --> _int)))


(define-ffi-accessor integrity-ffi-get-chf-count!
  (crypto-get-obj! #"XIDEN_AVAILABLE_CHF_COUNT"
                   _uint))


(define-ffi-accessor integrity-ffi-get-supported-chfs!
  (crypto-get-obj! #"XIDEN_SUPPORTED_CHFS"
                   (_array _string (integrity-ffi-get-chf-count!))))


(define-ffi-procedure (integrity-ffi-get-c-chfs!)
  (define chf-count (integrity-ffi-get-chf-count!))
  (define available (integrity-ffi-get-supported-chfs!))
  (if (and chf-count available)
      (for/list ([i (in-range chf-count)])
        (string->symbol (string-downcase (array-ref available i))))
      null))


(define-ffi-procedure (integrity-ffi-find-default-chf!)
  (define default-index (integrity-ffi-get-default-chf-index!))
  (and default-index
       (list-ref (integrity-ffi-get-c-chfs!)
                 default-index)))


(define-ffi-procedure (integrity-ffi-make-digest! in algorithm)
  (define p-md (integrity-ffi-load-chf! algorithm))
  (define digest-size ((integrity-ffi-get-get-digest-size!) p-md))
  (define read-buffer-size (* 128 1024))
  (define gc-buffer (make-bytes read-buffer-size))
  (define digest-buffer (make-bytes digest-size))

  (define (read-more p-size read-so-far)
    (define bytes-read
      (read-bytes! gc-buffer
                   in
                   0
                   read-buffer-size))
    (and (not (eof-object? bytes-read))
         (begin (ptr-set! p-size _uint bytes-read)
                gc-buffer)))

  (define status
    ((integrity-ffi-get-make-digest!) p-md digest-buffer read-more))

  (if (equal? status 1)
      digest-buffer
      (crypto-raise!)))


(define-ffi-procedure (integrity-ffi-load-chf! chf)
  (define mdindex (index-of (integrity-ffi-get-c-chfs!) chf))
  (if mdindex
      (let ([p_md ((integrity-ffi-get-load-chf!) mdindex)])
        (unless p_md (raise (crypto-raise!)))
        p_md)
      (raise ($chf-unavailable chf))))


(module+ test
  (provide integrity-ffi-operational?)

  (require rackunit
           "../codec.rkt"
           (submod "chf.rkt" test)
           (only-in racket/string non-empty-string?)
           (only-in ffi/unsafe array-ref))

  (define integrity-ffi-operational?
    (integrity-ffi-available?!))

  (when integrity-ffi-operational?
    (define default-chf-index (integrity-ffi-get-default-chf-index!))
    (define load-chf (integrity-ffi-get-load-chf!))
    (define get-digest-size (integrity-ffi-get-get-digest-size!))
    (define make-digest (integrity-ffi-get-make-digest!))
    (define chf-count (integrity-ffi-get-chf-count!))
    (define supported-chfs (integrity-ffi-get-supported-chfs!))
    (define supported-chfs/symbols (integrity-ffi-get-c-chfs!))
    (define default-chf (integrity-ffi-find-default-chf!))

    (check-true (> (hash-count private-table) 0))
    (check-pred exact-positive-integer? chf-count)
    (check-pred exact-nonnegative-integer? default-chf-index)
    (for ([i (in-range chf-count)])
      (check-pred non-empty-string? (array-ref supported-chfs i)))
    (for ([sym (in-list supported-chfs/symbols)])
      (check-pred integrity-ffi-chf-available?! sym))
    (check-false (integrity-ffi-chf-available?! '||))

    (check-pred symbol? default-chf)
    (check-pred integrity-ffi-chf-available?! default-chf)

    (for ([pair (in-list test-digests)])
      (define chf (car pair))
      (define expected-digest (base64 (cdr pair)))
      (with-handlers ([$crypto:error? (λ ($) (fail))])
        (test-equal? (format "Create digest using CHF ~a" chf)
                     (integrity-ffi-make-digest! (open-input-bytes test-digest-data) chf)
                     expected-digest)))))
