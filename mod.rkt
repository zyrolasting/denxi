#lang racket/base

; Define a plugin system such that an administrator-provided module
; provides functionality required by other parts of Xiden.
;
; Warning: Plugins run in Xiden's process.

(require racket/contract)

(provide
 (contract-out
  [load-plugin
   (-> symbol? (-> any/c) (-> exn? any) any/c)]))

(require "rc.rkt")

(define (load-plugin key fail-thunk on-load-failure)
  (define maybe-path (XIDEN_PLUGIN_MODULE))
  (if maybe-path
      (with-handlers ([exn:fail? on-load-failure])
        (dynamic-require maybe-path
                         key
                         fail-thunk))
      (fail-thunk)))
