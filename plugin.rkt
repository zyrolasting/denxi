#lang racket/base

; Define a plugin system such that an administrator-provided module
; provides functionality required by other parts of Xiden.
;
; Warning: Plugins run in Xiden's process.

(require racket/contract)

(provide
 (contract-out
  [plugin-ref
   (-> symbol? any/c any/c)]
  [load-from-plugin
   (-> symbol? (-> any/c) (-> exn? any) any/c)]))

(require (only-in racket/function const)
         "strict-rc.rkt")

(define (load-from-plugin key fail-thunk on-load-failure)
  (define maybe-path (rc-ref 'XIDEN_PLUGIN_MODULE))
  (if maybe-path
      (with-handlers ([exn:fail? on-load-failure])
        (dynamic-require (if (string? maybe-path)
                             (string->path maybe-path)
                             maybe-path)
                         key
                         fail-thunk))
      (fail-thunk)))

(define (plugin-ref key default-value)
  (load-from-plugin key
                    (const default-value)
                    (const default-value)))

(module+ test
  (provide call-with-plugin)
  (require racket/file
           rackunit)

  (define (call-with-plugin plugin proc)
    (define plugin-path (make-temporary-file))
    (dynamic-wind void
                  (λ ()
                    (write-to-file #:exists 'truncate/replace plugin plugin-path)
                    (rc-rebind 'XIDEN_PLUGIN_MODULE plugin-path proc))
                  (λ ()
                    (delete-file plugin-path))))

  (define wont-load (find-system-path 'temp-dir))
  
  (test-exn "Handle plugin load failures"
            exn:fail:filesystem?
            (λ ()
              (rc-rebind 'XIDEN_PLUGIN_MODULE wont-load
               (λ ()
                 (load-from-plugin 'whatever
                                   (λ () (fail "Wrong fail thunk called"))
                                   raise)
                 (check-equal? (plugin-ref 'whatever 'fallback) 'fallback)))))

  (test-case "Work with dummy plugins"
    (call-with-plugin
     '(module anon racket/base (provide a) (define a 1))
     (λ ()
       (check-equal? (plugin-ref 'a 2) 1)
       (check-equal? (plugin-ref 'b 2) 2)))))
