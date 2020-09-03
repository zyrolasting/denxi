#lang racket/base

; Define a module language for derivations. Adapted from setup/infotab
; https://raw.githubusercontent.com/racket/racket/master/racket/collects/setup/infotab.rkt

(require racket/contract)

(provide #%app #%datum #%top #%top-interaction
         define define-values
         error
         eval
         hash
         hash-ref
         lambda λ
         let let-values
         quote quasiquote unquote unquote-splicing
         if
         cond
         case
         list cons car cdr list* append reverse
         equal?
         from-file
         init!
         make-immutable-hash hash hash-set hash-set* hash-remove hash-clear hash-update
         string-append
         path->string
         build-path
         collection-path
         system-library-subpath
         getenv
         (all-from-out "archiving.rkt"
                       file/untgz)
         (rename-out [#%module-begin* #%module-begin]
                     [list sources])
         (contract-out
          [cd
           (-> path? void?)]
          [input
           (->* (non-empty-string?
                 (non-empty-listof path-string?))
                ((or/c #f integrity-info?)
                 (or/c #f signature-info?))
                input-info?)]

          [input-ref
           (-> input-info?
               complete-path?)]

          [from-package
           (-> string?
               (listof url-string?))]

          [integrity
           (-> xiden-hash-algorithm/c
               bytes?
               integrity-info?)]

          [signature
           (-> xiden-cipher-algorithm/c
               (or/c bytes? path-string?)
               bytes?
               signature-info?)]

          [fingerprint
           (-> (or/c non-empty-string? bytes?) bytes?)]

          [base32
           (-> (or/c non-empty-string? bytes?) bytes?)]

          [base64
           (-> (or/c non-empty-string? bytes?) bytes?)]

          [hex
           (-> (or/c non-empty-string? bytes?) bytes?)]))



(require (for-syntax racket/base
                     syntax/location
                     syntax/parse)
         (only-in file/sha1 hex-string->bytes)
         file/untgz
         racket/function
         racket/sequence
         racket/string
         "archiving.rkt"
         "encode.rkt"
         "compression.rkt"
         "format.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "message.rkt"
         "monad.rkt"
         "output-info.rkt"
         "path.rkt"
         "printer.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "url.rkt")


; Must be called in sandbox.rkt to pass along configuration.
; TODO: Find a way to prevent use in top-level of #lang xiden module
(define init!
  (let ([called? #f])
    (λ (dump)
      (if called? (void)
          (begin (for ([(k v) (in-hash XIDEN_SETTINGS)])
                   ((setting-derived-parameter v) (hash-ref dump k)))
                 (set! called? #t))))))

(define (cd path)
  (current-directory path))

(define (input name sources [integrity #f] [signature #f])
  (input-info name sources integrity signature))

(define format-message
  (combine-message-formatters format-input-message
                              format-fetch-message
                              default-message-formatter))

(define (input-ref input)
  (displayln (XIDEN_TRUST_UNSIGNED))
  (write-message ($input-resolve-start (input-info-name input)) format-message)
  (define-values (path messages) (run-log (resolve-input input) null))
  (sequence-for-each
   (λ (m) (write-message m format-message))
   (in-list (reverse messages)))
  (if (eq? path FAILURE)
      (raise-user-error 'input-ref
                        "Could not resolve input ~a~nSources:~n~a~n"
                        (input-info-name input)
                        (join-lines (map ~a (input-info-sources input))))
      path))


(define-syntax (from-file stx)
  (syntax-parse stx
    [(_ user-path:string)
     (for ([el (in-list (explode-path (syntax-e #'user-path)))])
       (when (eq? el 'up)
         (raise-syntax-error 'from-file
                             "A relative path source may not reference parent directories."
                             #'user-path)))
     (with-syntax ([wrt (syntax-source-directory stx)])
       #'(normalize-path user-path wrt))]))


(define (from-package query-string)
  (map url->string
       (map/service-endpoints query-string
                              (XIDEN_SERVICE_ENDPOINTS))))

(define (output name expr)
  (cons name expr))

(define integrity integrity-info)
(define signature signature-info)
(define fingerprint values)
(define base32 (curry decode 'base32))
(define base64 (curry decode 'base64))
(define (hex variant)
  (hex-string->bytes (if (string? variant) variant (bytes->string/utf-8 variant))))

(define-syntax (#%module-begin* stx)
  (syntax-case stx ()
    [(mod-beg defn ...)
     (let ([names (let loop ([defns (syntax->list #'(defn ...))]
                             [r '()])
                    (if (null? defns)
                        (reverse r)
                        (loop (cdr defns)
                              (syntax-case (car defns) (define)
                                [(define var val)
                                 (identifier? #'var)
                                 (cons #'var r)]
                                [(define (var) val ...)
                                 (identifier? #'var)
                                 (cons #'var r)]
                                [(define (var s ...) val ...)
                                 (identifier? #'var)
                                 (cons #'var r)]
                                [(define-values (var) val)
                                 (cons #'var r)]
                                [_else (raise-syntax-error
                                        'xiden
                                        "not a well-formed package definition"
                                        stx (car defns))]))))])
       (let ([dup (check-duplicate-identifier names)])
         (when dup
           (raise-syntax-error 'xiden "duplicate definition" stx dup)))
       (with-syntax ([(name ...) names])
         #'(#%plain-module-begin
            defn ...
            (define #%info-lookup
              (case-lambda
                [(n) (#%info-lookup n (λ () (error 'xiden "no info for ~a" n)))]
                [(n fail)
                 (unless (and (procedure? fail)
                              (procedure-arity-includes? fail 0))
                   (error
                    'xiden
                    "expected second argument to be a procedure that takes no arguments, got: ~e"
                    fail))
                 (case n
                   [(name) name]
                   ...
                   [else (fail)])]))
            (define (#%info-domain) '(name ...))
            (provide #%info-lookup #%info-domain))))]))
