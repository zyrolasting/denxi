#lang racket/base

; Module language for package definitions

(require (for-syntax "pkgdef/expand.rkt" racket/base)
         "archive.rkt"
         "codec.rkt"
         "file.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "logged.rkt"
         (rename-in "monad.rkt" [do do*])
         "signature.rkt"
         "source.rkt"
         "system.rkt")

(provide apply
         base64
         base32
         current-directory
         hex
         input
         integrity
         signature
         in-paths
         logged-unit
         run
         sources
         from-catalogs
         from-file
         find-executable-path
         extract
         resolve-input
         release-input
         find-input
         <-
         void
         (except-out (all-from-out racket/base) do #%module-begin)
         (rename-out [#%module-begin* #%module-begin]
                     [do* do]))

(define-syntax (#%module-begin* stx)
  (syntax-case stx ()
    [(_ . body)
     (with-syntax ([expanded (expand-pkgdef-module #'body stx)])
       #'(#%module-begin expanded))]))
