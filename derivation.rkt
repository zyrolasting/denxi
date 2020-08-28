#lang racket/base

; Define a derivation and the operations used to create
; unique and internally-consistent directories on disk.

(require "contract.rkt")
(provide make-build-sandbox)

(require racket/function
         racket/path
         racket/runtime-path
         racket/sandbox
         "config.rkt"
         "exn.rkt"
         "file.rkt"
         "localstate.rkt"
         "message.rkt"
         "path.rkt"
         "rc.rkt"
         "setting.rkt"
         "source.rkt"
         "workspace.rkt")

(define-runtime-path module-language-path "derivation-forms.rkt")

(define+provide-message $consent-note ())
(define+provide-message $no-package-info (source))
(define+provide-message $sandbox ())
(define+provide-message $sandbox-crash (exn-string))
(define+provide-message $sandbox-error (line))
(define+provide-message $sandbox-output (line))


(define (make-derivation-module inputs outputs)
  `(module derivation ,module-language-path
     (define input-ref
       (let ([h ,inputs])
         (λ (key)
           (fetch-input (hash-ref h key (λ () (error "No such input: ~a" key)))))))
     (define build!
       (let ([h ,outputs])
         (λ (key)
           (eval (hash-ref h key (λ () (error "No such output: ~a" key)))))))))


(define (build-derivation input-program directory outputs)
  (define seval (make-build-sandbox input-program directory))
  (define pump-stdout (thread (make-pump $sandbox-output directory (get-output seval))))
  (define pump-stderr (thread (make-pump $sandbox-error  directory (get-error-output seval))))
  (dynamic-wind void
                (λ ()
                  (collect-sandbox-eval-results seval (map (λ (e) `(build! ,e) outputs))))
                (λ ()
                  (break-thread pump-stdout)
                  (break-thread pump-stderr))))


(define (collect-sandbox-eval-results seval expressions)
  (for/fold ([msg (:return)])
            ([expr (in-list expressions)]
             #:break ($with-messages-intermediate msg))
    (:merge msg
            (with-handlers ([values (λ (e) (attach-message #t ($sandbox-crash (exn->string e))))])
              (let ([out (seval expr)])
                (if ($with-messages? out)
                    out
                    (attach-message #f ($sandbox-output out))))))))


(define (make-pump message-envelope name port)
  (λ ()
    (with-handlers ([exn:break? (λ (e) (close-input-port port))])
      (let loop ()
        (emit-message! (message-envelope (read (sync port))))
        (loop)))))


; The sandbox mitigates the risk of rogue code, but the runtime
; configuration serves as an attack vector.
(define (make-build-sandbox input-program build-directory)
  (parameterize ([sandbox-output 'pipe]
                 [sandbox-error-output 'pipe]
                 [sandbox-input #f]
                 [sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-path-permissions (make-build-sandbox-path-permissions build-directory)]
                 [sandbox-make-environment-variables make-environment-variables])
    (make-module-evaluator #:allow-for-require (list "package-info-lang.rkt"
                                                     module-language-path)
                           #:language module-language-path
                           input-program)))


(define (make-build-sandbox-path-permissions build-directory)
  (list (list 'write build-directory)
        (list 'read "/etc/pki/tls") ; TODO: Find cross-platform expression
        (list 'read (get-objects-directory))
        (list 'exists (workspace-directory))))


(module+ test
  (require rackunit
           "file.rkt")

  (with-temporary-directory
    (build-derivation (current-directory)
                      '(call-with-output-file "foo"
                         (λ (o) (write-bytes #"bar" o))))))
