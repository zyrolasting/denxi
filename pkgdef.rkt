#lang racket/base

; Define a module language for derivations. Adapted from setup/infotab
; https://raw.githubusercontent.com/racket/racket/master/racket/collects/setup/infotab.rkt

(require racket/contract)

(provide λ
         #%app
         #%datum
         #%top
         #%top-interaction
         append
         base32
         base64
         build-path
         build-workspace-path
         call-with-input
         car
         case
         cdr
         collection-path
         cond
         cons
         current-directory
         define
         define-values
         equal?
         error
         from-catalogs
         from-file
         getenv
         hash
         hash-clear
         hash-ref
         hash-remove
         hash-set
         hash-set*
         hash-update
         hex
         if
         input
         input-ref
         install
         integrity
         keep-input!
         lambda
         let
         let-values
         list
         list*
         make-immutable-hash
         path->string
         quasiquote
         quote
         reverse
         signature
         sources
         string-append
         system-library-subpath
         unpack
         unquote
         unquote-splicing
         void
         (rename-out [#%module-begin* #%module-begin]))


(require (for-syntax racket/base)
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
         "logged.rkt"
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

(define (install link-path output-name pkgdef)
  (define result (run+print-log (run-package link-path output-name pkgdef)))
  (if (eq? result FAILURE)
      (raise-user-error 'install
                        "Could not install package output ~a"
                        output-name)
      (void)))

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
