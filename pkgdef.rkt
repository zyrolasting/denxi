#lang racket/base

; Module language for package definitions

(require (for-syntax "pkgdef/expand.rkt" racket/base)
         "codec.rkt"
         "file.rkt"
         "input-info.rkt"
         "integrity.rkt"
         (rename-in "monad.rkt" [do do*])
         "signature.rkt"
         "source.rkt"
         "system.rkt")

(provide base64
         base32
         hex
         input
         integrity
         signature
         in-paths
         run
         sources
         from-catalogs
         from-file
         <-
         (except-out (all-from-out racket/base) do #%module-begin)
         (rename-out [#%module-begin* #%module-begin]
                     [do* do]))

(define-syntax (#%module-begin* stx)
  (syntax-case stx ()
    [(_ . body)
     (with-syntax ([expanded (expand-pkgdef-module #'body stx)])
       #'(#%module-begin expanded))]))

