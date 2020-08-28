#lang racket/base

; Define a derivation and the operations used to create
; unique and internally-consistent directories on disk.

(require "contract.rkt")
(provide make-sandbox
         (contract-out
          [build-derivation
           (-> well-formed-derivation/c list?)]))


(require racket/function
         racket/path
         racket/pretty
         racket/sandbox
         racket/sequence
         net/head
         version/utils
         "config.rkt"
         "contract.rkt"
         "encode.rkt"
         "exn.rkt"
         "file.rkt"
         "integrity.rkt"
         "input-info.rkt"
         "localstate.rkt"
         "message.rkt"
         "mod.rkt"
         "package-info.rkt"
         "path.rkt"
         "port.rkt"
         "query.rkt"
         "racket-version.rkt"
         "rc.rkt"
         "setting.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "url.rkt"
         "openssl.rkt"
         "output-info.rkt"
         "workspace.rkt")


(define+provide-message $consent-note ())
(define+provide-message $no-package-info (source))
(define+provide-message $sandbox ())
(define+provide-message $sandbox-crash (exn-string))
(define+provide-message $sandbox-error (line))
(define+provide-message $sandbox-output (line))
(define+provide-message $unverified-host (url))

(struct derivation (inputs output) #:prefab)

(define well-formed-derivation/c
  (struct/c derivation
            (listof well-formed-input-info/c)
            well-formed-output-info/c))


(define (build-derivation drv)
  (:do #:with (fetch* (derivation-inputs drv) make-input-file)
       (λ (input-bindings)
         (make-refs input-bindings))
       (λ (_) (run-builder (derivation-output drv)))))


(define (make-refs bindings)
  (:merge (for/fold ([accum (:return)])
                    ([(link-name target-path) (in-hash bindings)])
            (:merge accum (make-link/clobber link-name target-path)))
          (:return (void))))


(define (run-builder output)
  (define name (output-info-builder-name output))
  (define seval (make-sandbox name))
  (define pump-stdout (thread (make-pump $sandbox-output name (get-output seval))))
  (define pump-stderr (thread (make-pump $sandbox-error  name (get-error-output seval))))
  (dynamic-wind void
                (λ () (collect-sandbox-eval-results seval (output-info-builder-expressions output)))
                (λ ()
                  (break-thread pump-stdout)
                  (break-thread pump-stderr))))


(define (collect-sandbox-eval-results seval expressions)
  (for/fold ([msg (:return)])
            ([expr (in-list expressions)]
             #:break ($with-messages-intermediate msg))
    (:merge msg
            (with-handlers ([values (λ (e) (attach-message #t ($sandbox-crash (exn->string e))))])
              (attach-message #f ($sandbox-output (seval expr)))))))


(define (make-pump message-envelope name port)
  (λ ()
    (with-handlers ([exn:break? (λ (e) (close-input-port port))])
      (let loop ()
        (emit-message! (message-envelope (read-line (sync port))))
        (loop)))))


; The sandbox is meant to guard against rogue setup modules. It
; assumes that the target module may build output inside of the
; directory in which it appears. This makes the surrounding app
; responsible for carefully selecting where the module appears.
(define (make-sandbox path build-directory input-files)
  (parameterize ([sandbox-output 'pipe]
                 [sandbox-error-output 'pipe]
                 [sandbox-input #f]
                 [sandbox-memory-limit (XIDEN_SANDBOX_MEMORY_LIMIT_MB)]
                 [sandbox-eval-limits (list (XIDEN_SANDBOX_EVAL_TIME_LIMIT_SECONDS)
                                            (XIDEN_SANDBOX_EVAL_MEMORY_LIMIT_MB))]
                 [sandbox-path-permissions (make-build-sandbox-path-permissions build-directory input-files)]
                 [sandbox-make-environment-variables make-environment-variables])
    (make-module-evaluator #:language 'racket/base path)))


(define (make-build-sandbox-path-permissions build-directory input-files)
  (append (map (λ (input-path) (list 'read input-path)) input-files)
          (cons (list 'write build-directory)
                (XIDEN_SANDBOX_PATH_PERMISSIONS))))

(module+ test
  (require rackunit))
