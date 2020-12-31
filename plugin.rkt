#lang racket/base

; Define a plugin system such that an administrator-provided module
; provides functionality required by other parts of Xiden.
;
; Warning: Plugins run in Xiden's process.

(require racket/contract)

(provide
 (contract-out
  [load-from-plugin
   (-> symbol? (-> any/c) (-> exn? any) any/c)]))

(require "rc.rkt")

(define (load-from-plugin key fail-thunk on-load-failure)
  (define maybe-path (XIDEN_PLUGIN_MODULE))
  (if maybe-path
      (with-handlers ([exn:fail? on-load-failure])
        (dynamic-require maybe-path
                         key
                         fail-thunk))
      (fail-thunk)))

(module+ test
  (provide call-with-plugin)
  (require racket/file
           rackunit)

  (define (call-with-plugin plugin proc)
    (define plugin-path (make-temporary-file))
    (dynamic-wind void
                  (λ ()
                    (write-to-file #:exists 'truncate/replace plugin plugin-path)
                    (XIDEN_PLUGIN_MODULE plugin-path proc))
                  (λ ()
                    (delete-file plugin-path)))))
