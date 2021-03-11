#lang racket/base

; Define a plugin system such that an administrator-provided module
; provides functionality required by other parts of Xiden.
;
; Warning: Plugins run in Xiden's process.

(require racket/contract
         racket/function
         "workspace.rkt")

(provide
 (contract-out
  [plugin-ref
   (-> symbol? any/c any/c)]
  [load-from-plugin
   (-> symbol? (-> any/c) (-> exn? any) any/c)]))


(define (get-plugin-path [wrt (workspace-directory)])
  (build-workspace-path "etc/xiden.rkt"))


(define (load-from-plugin key fail-thunk on-load-failure)
  (with-handlers ([exn:fail? on-load-failure])
    (dynamic-require (get-plugin-path) key fail-thunk)))


(define (plugin-ref key default-value)
  (let ([fail (const default-value)])
    (load-from-plugin key fail fail)))


(module+ test
  (provide call-with-plugin)
  (require racket/file
           racket/path
           rackunit)

  (define (call-with-plugin plugin proc)
    (call-with-ephemeral-workspace
     (λ (ws)
       (define plugin-path (get-plugin-path ws))
       (make-directory* (path-only plugin-path))
       (write-to-file #:exists 'truncate/replace plugin plugin-path)
       (proc))))

  (define wont-load (find-system-path 'temp-dir))

  (test-case "Define plugin path in terms of workspace"
    (define /a (parameterize ([workspace-directory "/a"]) (get-plugin-path)))
    (define /b (parameterize ([workspace-directory "/b"]) (get-plugin-path)))
    (check-pred complete-path? /a)
    (check-pred complete-path? /b)
    (check-not-equal? /a /b))
  
  (test-exn "Handle plugin load failures"
            exn:fail:filesystem?
            (λ ()
              (call-with-ephemeral-workspace
               (λ (ws)
                 (make-directory* (get-plugin-path ws))
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
