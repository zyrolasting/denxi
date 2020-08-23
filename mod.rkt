#lang racket/base

; Define a plugin system such that an administrator-provided module
; provides functionality required by other parts of Xiden.
;
; MODS ARE AN ATTACK VECTOR. Unlike the package management process
; itself, this implementation runs with the same privilege level as
; Xiden's user.

(require racket/contract
         racket/exn)

(provide
 (contract-out
  [load-plugin
   (-> symbol? (-> any/c) (-> exn? any) any/c)]
  #;[load-plugin/with-output
   (-> symbol? (-> any/c) $with-output?)]))

(require "output.rkt"
         "rc.rkt"
         "xiden-messages.rkt")

(define (load-plugin key fail-thunk on-load-failure)
  (define maybe-path (XIDEN_MODS_MODULE))
  (if maybe-path
      (with-handlers ([exn:fail? on-load-failure])
        (dynamic-require maybe-path
                         key
                         fail-thunk))
      (fail-thunk)))
