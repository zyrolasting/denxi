#lang racket/base

; Define a module language for derivations. Adapted from setup/infotab
; https://raw.githubusercontent.com/racket/racket/master/racket/collects/setup/infotab.rkt


(provide #%app #%datum #%top #%top-interaction
         define define-values
         error
         eval
         hash
         hash-ref
         lambda λ
         let let-values
         quote quasiquote unquote unquote-splicing
         (rename-out [#%module-begin* #%module-begin])
         if
         cond
         case
         list cons car cdr list* append reverse
         equal?
         make-immutable-hash hash hash-set hash-set* hash-remove hash-clear hash-update
         string-append
         path->string
         build-path
         collection-path
         system-library-subpath
         getenv)


(require (for-syntax racket/base)
         racket/contract
         "archiving.rkt"
         "encode.rkt"
         "compression.rkt"
         "localstate.rkt"
         "message.rkt"
         "rc.rkt"
         "source.rkt")

(define-syntax #%module-begin*
  (λ (stx)
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
                                   (cons (syntax var) r)]
                                  ;; In case it gets expanded:
                                  [(define-values (var) val)
                                   (cons (syntax var) r)]
                                  [_else (raise-syntax-error
                                          'xiden
                                          "not a well-formed package definition"
                                          stx (car defns))]))))])
         (let ([dup (check-duplicate-identifier names)])
           (when dup
             (raise-syntax-error 'xiden "duplicate definition" stx dup)))
         (with-syntax ([(name ...) names])
           (syntax
            (#%plain-module-begin
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
             (provide #%info-lookup #%info-domain)))))])))
