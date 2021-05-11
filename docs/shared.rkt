#lang racket

(provide (all-defined-out))

(require scribble/manual
         syntax/strip-context
         (for-syntax racket
                     syntax/stx
                     syntax/strip-context
                     xiden/setting
                     xiden/cli-flag
                     xiden/package)
         xiden/setting
         xiden/cli-flag)


(define (visible-hyperlink s)
  (hyperlink s s))

(define xiden-index
  '(lib "xiden/docs/index/xiden-index.scrbl"))

(define xiden-reference
  '(lib "xiden/docs/reference/xiden-reference.scrbl"))

(define xiden-guide
  '(lib "xiden/docs/guide/xiden-guide.scrbl"))

(define xiden-tutorials
  '(lib "xiden/docs/tutorials/xiden-tutorials.scrbl"))

(define xiden-practices
  '(lib "xiden/docs/practices/xiden-practices.scrbl"))

(define-for-syntax (reformat-syntax stx v)
  (replace-context stx
    (read-syntax #f (open-input-string (with-output-to-string (λ () (pretty-write v)))))))

(define (tech/reference tag)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") tag))

(define (tech/xiden-guide tag)
  (tech #:doc xiden-guide tag))

(define (tech/xiden-reference tag)
  (tech #:doc xiden-reference tag))

(define (tech/xiden-tutorials tag)
  (tech #:doc xiden-tutorials tag))

(define (tutorial tag)
  (secref #:doc xiden-tutorials tag))

(define (practice tag)
  (secref #:doc xiden-practices tag))

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
