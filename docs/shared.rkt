#lang racket

(provide (all-defined-out))

(require scribble/manual
         syntax/strip-context
         racket/format
         net/uri-codec
         (for-syntax racket
                     syntax/stx
                     syntax/strip-context
                     denxi/setting
                     denxi/cli-flag
                     denxi/package)
         denxi/setting
         denxi/cli-flag)

(define (search-term s)
  (hyperlink (format "https://duckduckgo.com/?q=~a" (uri-encode s))
             s))

(define (visible-hyperlink s)
  (hyperlink s s))

(define denxi-index
  '(lib "denxi/docs/index/denxi-index.scrbl"))

(define denxi-reference
  '(lib "denxi/docs/reference/denxi-reference.scrbl"))

(define denxi-guide
  '(lib "denxi/docs/guide/denxi-guide.scrbl"))

(define denxi-white-paper
  '(lib "denxi/docs/white-paper/denxi-white-paper.scrbl"))

(define foreign
  '(lib "scribblings/foreign/foreign.scrbl"))

(define-for-syntax (reformat-syntax stx v)
  (replace-context stx
    (read-syntax #f (open-input-string (with-output-to-string (λ () (pretty-write v)))))))

(define-syntax (by-slg stx)
  (syntax-case stx ()
    [_
     #'(para "Sage L. Gerard"
             (tt " (base64-decode #\"c2FnZUBzYWdlZ2VyYXJkLmNvbQ\")"))]))

(define (tech/reference tag)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") tag))

(define (tech/denxi-guide tag)
  (tech #:doc denxi-guide tag))

(define (tech/denxi-reference tag)
  (tech #:doc denxi-reference tag))

(define (tech/foreign tag)
  (tech #:doc foreign tag))

(define-for-syntax (infer-contract-expr stx s)
   (define proc (setting-valid? s))
   (define formatted (~v proc))
   (reformat-syntax stx
     (if (string-prefix? formatted "#<")
         (object-name proc)
         (read (open-input-string formatted)))))

(define-syntax (defsetting stx)
  (syntax-case stx ()
    [(_ s cnt pre-content ...)
      #`(let ([cf (find-cli-flag s)])
          (defthing
            #:kind "setting"
            s cnt
            #:value #,(datum->syntax stx (eval #'(s)) stx)
            (para "CLI Flags: "
                  (if cf
                      (litchar (format-cli-flags cf))
                      "N/A"))
            pre-content ...))]))

(define-syntax (defsetting* stx)
  (syntax-case stx ()
    [(_ s pre-content ...)
      #`(defsetting s #,(infer-contract-expr stx (eval #'s)) pre-content ...)]))
