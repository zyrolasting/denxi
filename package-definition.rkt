#lang racket/base

; Here, a package definition is the code for a Racket module as a
; syntax object or a list.

(require "contract.rkt")

(provide (contract-out
          [PACKAGE_DEFINITION_MODULE_LANG symbol?]
          [PACKAGE_DEFINITION_READER_LANG symbol?]
          [package-definition-variant/c flat-contract?]
          [package-definition-datum? predicate/c]
          [get-package-definition-body
           (-> package-definition-datum? list?)]
          [make-package-definition-datum
           (->* (list?) (#:id symbol?) package-definition-datum?)]
          [write-package-definition!
           (->* (package-definition-datum?
                 (or/c path-string? output-port?))
                (#:pretty? boolean?
                 #:exists (or/c 'error 'append 'update 'replace 'truncate 'truncate/replace))
                void?)]
          [read-package-definition
           (->* (package-definition-variant/c)
                syntax?)]))


(require (only-in racket/format ~s)
         (only-in racket/match match)
         (only-in racket/pretty pretty-write)
         (only-in syntax/modread
                  check-module-form
                  with-module-reading-parameterization)
         "exn.rkt"
         "string.rkt")


(define-exn exn:fail:xiden:read-package-definition exn:fail:xiden (variant for-module? lang))


;------------------------------------------------------------------------
; Definitions

(define (package-definition-datum? v)
  (match v
    [`(module ,_ ,(? (λ (v) (equal? PACKAGE_DEFINITION_MODULE_LANG v))) ,@_) #t]
    [_ #f]))

(define package-definition-variant/c
  (or/c path? syntax? list? string? bytes? input-port?))

(define PACKAGE_DEFINITION_MODULE_LANG 'xiden/pkgdef)
(define PACKAGE_DEFINITION_READER_LANG 'xiden)


;------------------------------------------------------------------------
; Package definition input

(define (read-package-definition variant)
  (cond [(path? variant) (call-with-input-file variant read-package-definition)]
        [(list? variant) (read-package-definition (~s variant))]
        [(syntax? variant) (read-package-definition (syntax->datum variant))]
        [(string? variant) (read-package-definition (open-input-string variant))]
        [(bytes? variant) (read-package-definition (open-input-bytes variant))]
        [(input-port? variant)
         (port-count-lines! variant)
         (with-handlers ([procedure? (λ (p) (raise (p variant)))])
           (define checked (check-module-form (read-module variant) '_ (get-source-v variant)))
           (syntax-case checked ()
             [(module id ml xs ...)
              (let ([module-lang (syntax-e #'ml)])
                (if (equal? module-lang PACKAGE_DEFINITION_MODULE_LANG)
                    checked
                    (rex exn:fail:xiden:read-package-definition
                         variant
                         #t
                         module-lang)))]))]))


; Returns a value suitable for use as a `src' argument in
; `read-syntax', or a `source-v' argument in `check-module-form'.
(define (get-source-v in)
  (define objname (object-name in))
  (if (and (path-string? objname)
           (file-exists? objname))
      objname
      #f))


(define (read-module in)
  (with-module-reading-parameterization
    (λ ()
      (parameterize ([current-reader-guard reader-guard])
        (read-syntax (get-source-v in)
                     in)))))


(define (reader-guard v)
  (if (equal? v `(submod ,PACKAGE_DEFINITION_READER_LANG reader))
      v
      (raise (λ (variant) ((exc exn:fail:xiden:read-package-definition variant #f v))))))


;------------------------------------------------------------------------
; Package definition output

(define (write-package-definition! #:pretty? [pretty? #t] #:exists [exists 'error] pkgdef variant)
  (cond [(path-string? variant)
         (call-with-output-file #:exists exists variant
           (λ (o) (write-package-definition! #:pretty? pretty? pkgdef o)))]
        [(output-port? variant)
         (parameterize ([print-reader-abbreviations #t])
           (if pretty?
               (begin (fprintf variant "#lang ~a~n" PACKAGE_DEFINITION_READER_LANG)
                      (for ([defn (in-list (get-package-definition-body pkgdef))])
                        (pretty-write #:newline? #t defn variant)))
               (pretty-write #:newline? #t pkgdef variant)))]))


;------------------------------------------------------------------------
; Construction/destructuring

(define (make-package-definition-datum #:id [id 'pkgdef] body)
  `(module ,id ,PACKAGE_DEFINITION_MODULE_LANG . ,body))


(define (get-package-definition-body datum)
  (match datum
    [`(module ,_ ,(? (λ (v) (equal? PACKAGE_DEFINITION_MODULE_LANG v)) _) ,xs ...)
     (match xs
       [`((#%module-begin ,body ...)) body]
       [_ xs])]))



(module+ test
  (require racket/file
           racket/function
           racket/port
           rackunit)


  (test-case "Detect package definition forms"
    (check-true (package-definition-datum? (make-package-definition-datum '(a b c))))
    (check-true (package-definition-datum? (make-package-definition-datum null)))
    (check-false (package-definition-datum? '(module anon something)))
    (check-false (package-definition-datum? `(module ,PACKAGE_DEFINITION_MODULE_LANG)))
    (check-false (package-definition-datum? '(module anon something 1 2 3))))

  (test-case "Extract body from package definition module forms"
    (check-equal? (get-package-definition-body
                   `(module anon ,PACKAGE_DEFINITION_MODULE_LANG a b c))
                  '(a b c))
    (check-equal? (get-package-definition-body
                   `(module anon ,PACKAGE_DEFINITION_MODULE_LANG (#%module-begin a b c)))
                  '(a b c))
    (check-equal? (get-package-definition-body
                   (make-package-definition-datum '(a b c)))
                  '(a b c)))

  (test-exn "Raise error on improper module form"
            exn:fail:syntax?
            (λ () (read-package-definition "(module)")))

  (test-exn "Raise error if EOF comes too early"
            exn:fail:syntax?
            (λ () (read-package-definition "")))

  (test-case "Read config with reader extension"
    (check-pred syntax? (read-package-definition "#lang xiden (define table #hash((\"coolio\" . marked)))")))

  (test-case "Accept only prescribed reader extensions"
    (define buffer (open-input-string "#lang racket/base (define table #hash((\"coolio\" . marked)))"))
    (check-exn
     (λ (e)
       (match e
         [(exn:fail:xiden:read-package-definition _ _ (? (λ (v) (eq? v buffer)) _) #f '(submod racket/base reader)) #t]))
     (λ ()
       (read-package-definition buffer))))

  (test-case "Do not accept anything other than expander language"
    (define buffer (open-input-string "(module content racket/base (define a 1))"))
    (check-exn
     (λ (e)
       (match e
         [(exn:fail:xiden:read-package-definition _ _ (? (λ (v) (eq? v buffer)) _) #t 'racket/base) #t]))
     (λ ()
       (read-package-definition buffer))))

  (test-case "Exchange package definition with filesystem"
    (define f (make-temporary-file))
    (define body '((define a 1) (define b 2)))
    (dynamic-wind
      void
      (λ ()
        (for ([pretty? (in-list '(#t #f))])
          (define mod (make-package-definition-datum body))
          (write-package-definition! #:exists 'truncate/replace #:pretty? #t mod f)
          (check-equal? (get-package-definition-body (syntax->datum (read-package-definition f))) body)))
      (λ () (delete-file f)))))
