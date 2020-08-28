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
(define+provide-message $input (info))
(define+provide-message $input-integrity $input (source))
(define+provide-message $input-integrity-assumed  $input-integrity ())
(define+provide-message $input-integrity-mismatch $input-integrity ())
(define+provide-message $input-integrity-verified $input-integrity ())
(define+provide-message $input-signature $input (source))
(define+provide-message $input-signature-mismatch $input-signature ())
(define+provide-message $input-signature-missing $input-signature ())
(define+provide-message $input-signature-trust-unsigned $input-signature ())
(define+provide-message $input-signature-unchecked $input-signature ())
(define+provide-message $input-signature-verified $input-signature ())
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
  (:do #:with (fetch-inputs (derivation-inputs drv))
       (λ (input-bindings) (make-refs input-bindings))
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


(define (fetch-inputs inputs)
  (apply :do #:with (hash)
         (for/list ([input (in-list inputs)])
           (λ (links)
             (define fetch-res (fetch-input input))
             (:merge fetch-res
                     (:return
                      (hash-set links
                                (input-info-name input)
                                ($with-messages-intermediate fetch-res))))))))


(define (fetch-input input request-transfer)
  (apply :do (map (λ (source) (fetch-exact-source input source))
                  (get-input-sources input))))

; I add any cached path as a source so that it goes through
; validation. This captures local tampering.
(define (get-input-sources input)
  (with-handlers ([exn? (λ _ (input-info-sources input))])
    (define path
      (build-object-path
       (integrity-info-digest
        (input-info-integrity input))))
    (if (file-exists? path)
        (cons (path->string path)
              (input-info-sources input))
        (input-info-sources input))))


(define (fetch-exact-source input source request-transfer)
  (:do #:with (fetch-source source request-transfer)
       (λ (path) (check-input-integrity input source path))
       (λ (path) (check-input-signature input source path))))


(define (check-input-integrity input source path)
  (if (XIDEN_TRUST_BAD_DIGEST)
      (attach-message path ($input-integrity-assumed input source))
      (if (check-integrity (input-info-integrity input) path)
          (attach-message path ($input-integrity-verified input source))
          (raise (attach-message #f ($input-integrity-mismatch input source))))))


(define (check-input-signature input source path)
  (if (XIDEN_TRUST_BAD_DIGEST)
      (attach-message path ($input-signature-unchecked input source))
      (if (XIDEN_TRUST_UNSIGNED)
          (attach-message path ($input-signature-trust-unsigned input source))
          (if (check-signature (integrity-info-digest (input-info-integrity input))
                               (signature-info-body (input-info-signature input))
                               (signature-info-pubkey (input-info-signature input)))
              (attach-message path ($input-signature-verified input source))
              (raise (attach-message #f ($input-signature-mismatch input source)))))))



(module+ test
  (require rackunit))
