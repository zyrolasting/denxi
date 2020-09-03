#lang racket/base

(require "contract.rkt")

(provide (all-from-out racket/sandbox)
         make-build-sandbox)

(require racket/function
         racket/path
         racket/sandbox
         "exn.rkt"
         "localstate.rkt"
         "message.rkt"
         "rc.rkt"
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
                 [sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-path-permissions (make-build-sandbox-path-permissions build-directory)]
                 [sandbox-make-environment-variables make-environment-variables])
    (make-module-evaluator #:language 'xiden/derivation-forms
                           input-program)))

(define (make-build-sandbox-path-permissions build-directory)
  (list (list 'write build-directory)
        (list 'read "/etc/pki/tls") ; TODO: Find cross-platform expression
        (list 'read (get-objects-directory))
        (list 'exists (workspace-directory))))
