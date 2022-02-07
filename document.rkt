#lang racket/base

(provide (all-defined-out)
         (for-label (all-from-out racket))
         (all-from-out racket
                       scribble/manual))

(require (rename-in scribble/manual [link scribble:link])
         syntax/strip-context
         net/uri-codec
         (except-in racket link)
         (for-label racket)
         (for-syntax racket/base
                     syntax/stx
                     syntax/strip-context))

(define-syntax (by-slg stx)
  (syntax-case stx ()
    [_
     #'(para "Sage L. Gerard"
             (tt " (base64-decode #\"c2FnZUBzYWdlZ2VyYXJkLmNvbQ\")"))]))

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

(define (tech/reference tag)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") tag))

(define (tech/denxi-guide tag)
  (tech #:doc denxi-guide tag))

(define (tech/denxi-reference tag)
  (tech #:doc denxi-reference tag))

(define (tech/foreign tag)
  (tech #:doc foreign tag))
