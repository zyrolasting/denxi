#lang racket/base

; Responsible for helping others understand the purpose and release
; information of a package.

(provide declare-info-i/o
         (all-from-out racket/sandbox
                       racket/string))

(require racket/contract
         racket/function
         racket/format
         racket/sandbox
         racket/string
         idiocket/exn
         (for-syntax racket/base
                     racket/format
                     racket/list
                     racket/match
                     racket/struct-info
                     racket/string
                     racket/syntax
                     syntax/parse))

; Expand to procedures that integrate struct definitions with #lang info files.
(define-syntax (declare-info-i/o stx)
  (define (coerce-string stx)
    (if (identifier? stx)
        (symbol->string (syntax->datum stx))
        (~a stx)))

  (syntax-parse stx
    [(_ struct-id:id)
     (match-define (list _ ctor _ accessors _ supertype)
       (extract-struct-info (syntax-local-value #'struct-id)))

     (define ctor-str (~a (coerce-string ctor) "-"))
     (define super-str (~a (coerce-string supertype) "-"))

     (define (accessor-id->field-name acc)
       (string->symbol (string-replace
                        (string-replace (coerce-string acc) ctor-str "")
                        super-str
                        "")))

     (with-syntax ([reader (format-id stx "read-~a" #'struct-id)]
                   [writer (format-id stx "write-~a" #'struct-id)]
                   [ctor-patt ctor]
                   [(accessor-patt ...) (reverse accessors)]
                   [(info-patt ...) (map accessor-id->field-name (reverse accessors))])
       #'(begin
           (define (reader path)
             (define i ((make-evaluator 'racket/base) `(dynamic-require ,path '#%info-lookup)))
             (define (l k) (with-handlers ([exn:fail? (const #f)]) (i k)))
             (ctor-patt (l 'info-patt) ...))
           (define (writer instance [out (current-output-port)])
             (parameterize ([current-output-port out])
               (displayln "#lang info\n")
               (begin
                 (writeln `(define info-patt ,(accessor-patt instance)))
                 ...)))))]))
