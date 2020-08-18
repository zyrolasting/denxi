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
  [dynamic-require/mod
   (-> symbol? (-> any/c) any/c)]))

(require "output.rkt"
         "printer.rkt"
         "rc.rkt"
         "xiden-messages.rkt")

(define sent-load-error? (box #f))

(define (dynamic-require/mod key fail-thunk)
  (define maybe-path (XIDEN_MODS_MODULE))
  (if maybe-path
      (with-handlers ([exn:fail?
                       (λ (e)
                         (unless (unbox sent-load-error?)
                           (write-output ($mod-load-failure maybe-path (exn->string e)))
                           (set-box! sent-load-error? #t))
                         (fail-thunk))])
        (dynamic-require maybe-path key fail-thunk))
      (fail-thunk)))
