#lang racket/base

; Convert sources of metadata to instances of a given structure.
;
; WARNING: This means that structure names and field names are part of
; more interfaces than just module exports.

(provide declare-info-i/o
         (all-from-out idiocket/sandbox
                       idiocket/string))

(require idiocket/contract
         idiocket/exn
         idiocket/function
         idiocket/port
         idiocket/sandbox
         idiocket/string
         net/head
         (for-syntax racket/base
                     racket/format
                     racket/list
                     racket/match
                     racket/struct-info
                     racket/string
                     racket/syntax
                     syntax/parse))

; Return a procedure that looks up a value by an identifier.
;
; Assume two sources:
;
; - A path to a `#lang info` file.
; - An input port with raw HTTP header information.
;
(define (make-lookup-procedure variant)
  (cond [(path? variant)
         (define i ((make-evaluator 'racket/base) `(dynamic-require ,variant '#%info-lookup)))
         (λ (k) (with-handlers ([exn:fail? (const #f)]) (i k)))]

        [(input-port? variant)
         (define headers (extract-all-fields (port->string variant)))
         (λ (v)
           (define pair (assoc (if (string? v) v (symbol->string v)) headers))
           ; Assume the HTTP header value is (read)able.
           ; If you want a string, then quote the value.
           (and pair (read (open-input-string (cdr pair)))))]
        [else (error (format "Cannot read metadata source: ~a" variant))]))


; Expand to procedures that integrate a struct definition with lookup procedures.
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
                   [get-info (format-id stx "getinfo/~a" #'struct-id)]
                   [ctor-patt ctor]
                   [(accessor-patt ...) (reverse accessors)]
                   [(abbrev-patt ...) (map accessor-id->field-name (reverse accessors))])
       #'(begin
           (define (reader variant)
             (define l (make-lookup-procedure variant))
             ; A metadata name can be just a field name, or the
             ; complete accessor name. The field names allow for
             ; abbreviated identifiers for `#lang info` files.  The
             ; full accessor names are for unambiguous metadata items.
             (ctor-patt (or (l 'abbrev-patt) (l 'accessor-patt)) ...))
           (define (get-info p)
             (define f (build-path p "info.rkt"))
             (and f (reader f)))
           (define (writer instance [out (current-output-port)])
             (parameterize ([current-output-port out])
               (displayln "#lang info\n")
               (begin
                 (writeln `(define info-patt ,(accessor-patt instance)))
                 ...)))))]))
