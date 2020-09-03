#lang racket/base

(require "contract.rkt")

(provide (all-from-out racket/sandbox)
         make-build-sandbox)

(require racket/function
         racket/path
         racket/sandbox
         racket/string
         "exn.rkt"
         "localstate.rkt"
         "message.rkt"
         "rc.rkt"
         "setting.rkt"
         "workspace.rkt")

(define+provide-message $sandbox ())
(define+provide-message $sandbox-crash (exn-string))
(define+provide-message $sandbox-error (line))
(define+provide-message $sandbox-output (line))

; The sandbox mitigates the risk of rogue code, but the runtime
; configuration is part of an attack vector. OS-level permissions
; are best.
(define (make-build-sandbox input-program build-directory)
  (parameterize ([sandbox-input #f]
                 [sandbox-output (current-output-port)]
                 [sandbox-error-output #f]
                 [sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-path-permissions (make-build-sandbox-path-permissions build-directory)]
                 [sandbox-make-environment-variables
                  (λ () (leak-environment-variables '(#"PATH")))])
    (define seval (make-module-evaluator #:language 'xiden/derivation-forms input-program))
    (seval `(init! ,(dump-xiden-settings) ,build-directory))
    seval))


(define (leak-environment-variables allowed)
  (apply make-environment-variables
         (for/fold ([mappings null])
                   ([name (in-list allowed)])
           (cons name
                 (cons (environment-variables-ref (current-environment-variables) name)
                       mappings)))))


; Allows access to some binaries in PATH
(define (make-bin-path-permissions binaries)
  (define windows? (eq? (system-type 'os) 'windows))
  (foldl (λ (s res)
           (append (map (λ (bin) (list 'execute (build-path s bin)))
                      binaries)
                   res))
         null
         (string-split (getenv "PATH")
                       (if windows? ";" ":"))))


(define (make-build-sandbox-path-permissions build-directory)
  (append (make-bin-path-permissions '("openssl"))
          (list (list 'write build-directory)
                (list 'write (build-workspace-path "var/xiden"))
                (list 'exists (workspace-directory))
                (list 'exists "/") ; Apparently some path normalization function needs this
                (list 'read "/etc/pki/tls") ; TODO: Find cross-platform expression
                (list 'read (get-objects-directory)))))
