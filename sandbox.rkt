#lang racket/base

; Extend racket/sandbox with operations to define and edit
; xiden/pkgdef modules. TODO: Redefine the interface such that saving
; a module requires syntax objects instead of loaded values from an
; evaluator.

(require "contract.rkt")
(provide (all-from-out racket/sandbox)
         (contract-out
          [derive-path-permissions
           (-> list?)]
          [dict->package-definition-datum
           (-> dict? list?)]
          [make-xiden-sandbox
           (-> (or/c syntax? pair? path? input-port? string? bytes?)
               (-> any/c any))]
          [bind-envvar-subset
           (-> (listof (or/c bytes? string?))
               (-> environment-variables?))]
          [hash+list->xiden-evaluator
           (->* (hash?) (list?) (-> any/c any))]
          [xiden-evaluator->hash
           (->* ((-> any/c any))
                ((listof symbol?))
                (hash/c symbol? any/c))]
          [xiden-evaluator-ref
           (->* ((-> any/c any/c)
                 symbol?)
                (failure-result/c)
                any/c)]
          [save-xiden-module!
           (->* ((-> any/c any)
                 (or/c path-string? output-port?))
                void?)]
          [write-xiden-module
           (->* ((-> any/c any) output-port?)
                (#:pretty? boolean?)
                void?)]
          [load-xiden-module
           (->* ((or/c syntax? path? string? bytes? input-port?))
                (-> any/c any))]))


(require (only-in racket/file
                  call-with-file-lock/timeout
                  make-lock-file-name
                  make-directory*
                  make-temporary-file)
         (only-in racket/dict dict? dict-keys dict-ref)
         (only-in racket/format ~s)
         (only-in racket/list remove-duplicates)
         (only-in racket/match match)
         (only-in racket/path path-only)
         (only-in racket/pretty pretty-write)
         racket/sandbox
         "string.rkt"
         "url.rkt")


(define (make-xiden-sandbox input-program)
  (parameterize ([sandbox-input #f]
                 [sandbox-output (current-output-port)]
                 [sandbox-error-output (current-error-port)])
    (make-module-evaluator #:language 'xiden/pkgdef input-program)))



(define (bind-envvar-subset allowed)
  (λ ()
    (apply make-environment-variables
           (for/fold ([mappings null])
                     ([unnormalized-name (in-list allowed)])
             (define name
               (if (string? unnormalized-name)
                   (string->bytes/utf-8 unnormalized-name)
                   unnormalized-name))
             (cons name
                   (cons (environment-variables-ref (current-environment-variables) name)
                         mappings))))))



;------------------------------------------------------------------------
; High-level module I/O


(define (save-xiden-module! seval variant)
  (cond [(path-string? variant) (save-local-xiden-module! seval variant)]
        [(output-port? variant) (write-xiden-module seval variant)]))


(define (load-xiden-module variant)
  (cond [(syntax? variant) (make-xiden-sandbox variant)]
        [(path? variant) (load-local-xiden-module variant)]
        [(list? variant) (load-xiden-module (open-input-infotab variant))]
        [(string? variant) (load-xiden-module (open-input-string variant))]
        [(bytes? variant) (load-xiden-module (open-input-bytes variant))]
        [(input-port? variant) (read-xiden-module variant)]))


(define (hash+list->xiden-evaluator h [l null])
  (define keys (remove-duplicates (append l (hash-keys h))))
  (load-xiden-module
   (~s `(module xinfotab xiden/pkgdef
          . ,(for/list ([k (in-list keys)])
               (define v (hash-ref h k))
               `(define ,k ,(if (list? v) `',v v)))))))


(define (xiden-evaluator->hash+list seval)
  (define domain (seval '(#%info-domain)))
  (values (xiden-evaluator->hash seval domain)
          domain))


(define (xiden-evaluator->hash seval [domain (seval '(#%info-domain))])
  (for/hash ([k (in-list domain)])
            (values k (seval `(#%info-lookup ',k)))))

(define (xiden-evaluator-ref seval sym [fail (λ () (error 'xiden-evaluator-ref "~a not set" sym))])
  (seval `(#%info-lookup ',sym ,(if (procedure? fail) fail (λ () fail)))))

;------------------------------------------------------------------------
; Module output

(define (write-xiden-module #:pretty? [pretty? #t] seval o)
  (parameterize ([print-reader-abbreviations #t])
    (if pretty?
        (print-pretty-module seval o)
        (pretty-write #:newline? #t
                      (make-infotab-module-datum seval)
                      o))))


(define (print-pretty-module seval o)
  (displayln "#lang xiden\n" o)
  (for ([k (in-list (seval '(#%info-domain)))])
    (define v (seval `(#%info-lookup ',k)))
    (pretty-write #:newline? #t `(define ,k ,(if (list? v) `',v v)) o)))


(define (save-local-xiden-module! seval path)
  (define lockfile (make-lock-file-name path))
  (define leading/ (path-only path))
  (when leading/
    (make-directory* leading/))
  (call-with-file-lock/timeout
   path 'exclusive
   (λ ()
     (call-with-output-file #:exists 'truncate/replace
       path (λ (o) (save-xiden-module! seval o))))
   (λ () (error 'save-local-xiden-module!
                "Failed to obtain lock for ~a"
                path)))
  (delete-file lockfile))


(define (dict->package-definition-datum dict)
  `(module xinfotab xiden/pkgdef
     . ,(for/list ([k (in-list (sort (dict-keys dict) symbol<?))])
          `(define ,k ,(dict-ref dict k)))))

(define (make-infotab-module-datum seval)
  `(module xinfotab xiden/pkgdef
     . ,(for/list ([k (in-list (seval '(#%info-domain)))])
          (define v (seval `(#%info-lookup ',k)))
          `(define ,k ,(if (list? v) `',v v)))))


(define (open-input-infotab l)
  (match l
    [(? null? l)
     (open-input-string "")]
    [(list 'module _ 'xiden/pkgdef _ ...)
     (open-input-string (~s l))]
    [(? dict? l)
     (open-input-infotab (dict->package-definition-datum l))]
    [_ (raise-argument-error 'open-input-infotab
                             "A valid list used for configuration"
                             l)]))



;------------------------------------------------------------------------
; Module input cases


(define (read-xiden-module in)
  (port-count-lines! in)
  (parameterize ([sandbox-path-permissions (derive-path-permissions)])
    (make-xiden-sandbox in)))

; Use cache because filesystem-root-list may take a while on Windows.
(define derive-path-permissions
  (let ([cache #f])
    (λ ()
      (unless cache
        (set! cache
              (append (map (λ (p) `(exists ,p)) (filesystem-root-list))
                      (sandbox-path-permissions))))
      cache)))

(define (load-local-xiden-module path)
  (parameterize ([sandbox-path-permissions
                  (let ([pp (sandbox-path-permissions)])
                    (cons `(read ,path) pp))])
    (call-with-input-file path read-xiden-module)))


(module+ test
  (require racket/function
           racket/port
           rackunit)

  (define expected-read-order
    '(who with in at))

  (define expected-hash-table
    (hasheq 'who "the butler"
            'with "the knife"
            'in "cellar"
            'at "12 pm"))

  (define dummy-module
    '(module x xiden/pkgdef
       (define who "the butler")
       (define with "the knife")
       (define in "the cellar")
       (define at "12 pm")))


  (define closure
    (hash+list->xiden-evaluator expected-hash-table
                                expected-read-order))

  (test-equal? "Access hash table"
               (hasheq 'who (closure '(#%info-lookup 'who))
                       'with (closure '(#%info-lookup 'with))
                       'in (closure '(#%info-lookup 'in))
                       'at (closure '(#%info-lookup 'at)))
               expected-hash-table)

  (test-equal? "Access read order"
               (closure '(#%info-domain))
               expected-read-order)

  (test-equal? "Access keyed values with a closure"
               (closure 'who)
               (hash-ref expected-hash-table 'who))

  (test-exn "Raise exn:fail on non-existant key"
            exn:fail?
            (λ () (closure 'why)))

  (test-exn "Raise error on improper module form"
            #rx"expecting a `module' program; got \\(module\\)"
            (λ () (load-xiden-module "(module)")))

  (test-exn "Raise error if EOF comes too early"
            #rx"expecting a single `module' program; no program expressions given"
            (λ () (load-xiden-module "")))

  (test-exn "Show position information for error"
            #rx"string:4:"
            (λ () (load-xiden-module
                   (string-join
                    '("(module content xiden/pkgdef"
                      "  (define a 1)"
                      "  (define b 2)"
                      "  (define c))")
                    "\n"))))

  (test-case "Save and load equal config"
    (define buffer (open-output-bytes))
    (define seval (load-xiden-module (~s dummy-module)))
    (save-xiden-module! seval buffer)
    (define-values (h0 l0) (xiden-evaluator->hash+list seval))
    (define-values (h1 l1) (xiden-evaluator->hash+list (load-xiden-module (get-output-bytes buffer))))
    (check-equal? h0 h1)
    (check-equal? l0 l1))

  (test-case "Can save and load from disk"
    (define tmp-file (make-temporary-file))
    (dynamic-wind
      void
      (λ ()
        (save-xiden-module! closure tmp-file)
        (define-values (h0 l0) (xiden-evaluator->hash+list closure))
        (define-values (h1 l1) (xiden-evaluator->hash+list (load-xiden-module tmp-file)))
        (check-equal? h0 h1)
        (check-equal? l0 l1))
      (λ () (delete-file tmp-file))))

  (test-case "Create infotab using a dictionary"
    (define expected '(module xinfotab xiden/pkgdef
                        (define a 1)
                        (define b '())))

    (define from-alist (dict->package-definition-datum '((a . 1) (b . '()))))
    (define from-hash  (dict->package-definition-datum (hash 'a 1 'b ''())))

    (check-equal? from-alist expected)
    (check-equal? from-hash expected))

  (test-case "Read literal infotab config"
    (define decl (make-infotab-module-datum (hash+list->xiden-evaluator (hash 'a 1 'b 2 'c 3) '(a b c))))
    (define lookup (load-xiden-module decl))
    (check-pred procedure? lookup)
    (check-eqv? (lookup 'a) 1)
    (check-eqv? (lookup 'b) 2)
    (check-eqv? (lookup 'c) 3))

  (test-case "Read abbreviated infotab config"
    (define lookup (load-xiden-module '((a . 1) (b . 2) (c . 3))))
    (check-pred procedure? lookup)
    (check-eqv? (lookup 'a) 1)
    (check-eqv? (lookup 'b) 2)
    (check-eqv? (lookup 'c) 3))

  (test-case "Read config with reader extension"
    (define buffer (open-input-string "#lang xiden (define table #hash((\"coolio\" . marked)))"))
    (define lookup (load-xiden-module buffer))
    (check-pred procedure? lookup)
    (check-equal? (lookup 'table) (hash "coolio" 'marked)))

  (test-exn "Accept only prescribed reader extensions"
            #rx"expecting `xiden/pkgdef"
            (λ ()
              (define buffer (open-input-string "#lang racket/base (define table #hash((\"coolio\" . marked)))"))
              (load-xiden-module buffer)))

  (test-exn "Do not accept anything other than expander language"
            #rx"expecting `xiden/pkgdef"
            (λ ()
              (define buffer (open-input-string "(module content racket/base (define a 1))"))
              (load-xiden-module buffer))))
