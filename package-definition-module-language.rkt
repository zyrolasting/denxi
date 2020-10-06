#lang racket/base

; Define a module language for derivations. Adapted from setup/infotab
; https://raw.githubusercontent.com/racket/racket/master/racket/collects/setup/infotab.rkt

(require racket/contract)

(provide #%app #%datum #%top #%top-interaction
         define define-values
         build-workspace-path
         current-directory
         current-info-lookup
         error
         eval
         hash
         hash-ref
         install
         lambda λ
         let let-values
         quote quasiquote unquote unquote-splicing
         if
         cond
         case
         list cons car cdr list* append reverse
         equal?
         from-file
         make-immutable-hash hash hash-set hash-set* hash-remove hash-clear hash-update
         void
         string-append
         path->string
         build-path
         collection-path
         system-library-subpath
         getenv
         (all-from-out "archiving.rkt"
                       "codec.rkt"
                       "integrity.rkt"
                       file/untgz)
         (rename-out [#%module-begin* #%module-begin]
                     [list sources])
         (contract-out
          [input
           (->* (non-empty-string?
                 (non-empty-listof path-string?))
                ((or/c #f integrity-info?)
                 (or/c #f signature-info?))
                input-info?)]

          [use-input
           (-> input-info?
               path-string?)]

          [input-ref
           (-> (listof input-info?)
               path-string?
               path-string?)]

          [from-catalogs
           (-> string?
               (listof url-string?))]

          [signature
           (-> (or/c bytes? path-string?)
               (or/c bytes? path-string?)
               signature-info?)]))


(require (for-syntax racket/base
                     syntax/location
                     syntax/parse)
         (only-in file/sha1 hex-string->bytes)
         (only-in net/uri-codec uri-encode)
         file/untgz
         racket/function
         racket/list
         racket/sequence
         racket/string
         "archiving.rkt"
         "codec.rkt"
         "compression.rkt"
         "format.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "l10n.rkt"
         "localstate.rkt"
         "message.rkt"
         "monad.rkt"
         "package.rkt"
         "path.rkt"
         "printer.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "url.rkt"
         "workspace.rkt")

(define current-info-lookup (make-parameter (λ (k f) (f))))

(define (input name sources [integrity #f] [signature #f])
  (input-info name sources integrity signature))


(define (input-ref inputs str)
  (define input (findf (λ (info) (equal? str (input-info-name info))) inputs))
  (if input
      (use-input input)
      (raise-user-error 'input-ref
                        "~s does not match a declared input"
                        str)))


(define (use-input input)
  (define path (run+print-log (resolve-input input)))
  (if (eq? path FAILURE)
      (raise-user-error 'input-ref
                        "Could not resolve input ~a~nSources:~n~a~n"
                        (input-info-name input)
                        (join-lines (map ~a (input-info-sources input))))
      path))


(define (install link-path output-name pkgdef)
  (define result (run+print-log (install-package link-path output-name pkgdef)))
  (if (eq? result FAILURE)
      (raise-user-error 'install
                        "Could not install package output ~a"
                        output-name)
      (void)))

(define (run+print-log logged-inst)
  (define-values (result messages) (run-log logged-inst))
  (define format-message (get-message-formatter))
  (sequence-for-each
   (λ (m) (write-message m format-message))
   (in-list (reverse (flatten messages))))
  result)

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


(define (from-catalogs query-string)
  (let ([encoded (uri-encode query-string)])
    (map (λ (url-string) (string-replace url-string "$QUERY" encoded))
         (XIDEN_CATALOGS))))

(define signature signature-info)

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
       (with-syntax ([(name ...) names]
                     [#%info-domain (datum->syntax stx '#%info-domain)]
                     [#%info-lookup (datum->syntax stx '#%info-lookup)])
         #'(#%plain-module-begin
            defn ...
            (define #%info-lookup
              (case-lambda
                [(n) (#%info-lookup n (λ () (error 'xiden "~a not set" n)))]
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
