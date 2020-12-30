#lang racket/base

; Module language for package definitions

(require (for-syntax "pkgdef/expand.rkt" racket/base)
         "archive.rkt"
         "codec.rkt"
         "file.rkt"
         "input-info.rkt"
         "integrity.rkt"
         "logged.rkt"
         "monad.rkt"
         "signature.rkt"
         "source.rkt"
         "system.rkt")

(provide base64
         base32
         current-directory
         hex
         input
         integrity
         signature
         in-paths
         logged-unit
         mdo
         run
         sources
         from-catalogs
         from-file
         extract
         resolve-input
         release-input
         :=
         void
         (rename-out [#%module-begin* #%module-begin]))

(define-syntax (#%module-begin* stx)
  (syntax-case stx ()
    [(_ . body)
     (with-syntax ([expanded (expand-pkgdef-module #'body stx)])
       #'(#%module-begin expanded))]))
