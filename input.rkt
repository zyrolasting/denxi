#lang racket/base

(require racket/contract)
(provide (struct-out package-input)
         (struct-out untrusted-source)
         (contract-out
          [abstract-package-input?
           flat-contract?]
          [make-package-input
           (->* (file-name-string?)
                (any/c)
                package-input?)]
          [resolve-input
           (-> package-input?
               (subprogram/c path-string?))]
          [release-input
           (-> package-input?
               (subprogram/c void?))]
          [keep-input
           (-> string?
               (subprogram/c path-string?))]
          [find-input
           (-> (listof package-input?)
               path-string?
               (subprogram/c package-input?))]
          [input-ref
           (-> string?
               (subprogram/c package-input?))]
          [find-artifact-for-input
           (-> package-input?
               (subprogram/c artifact?))]
          [current-inputs
           (parameter/c (listof package-input?))]))

(require racket/match
         racket/path
         "artifact.rkt"
         "dig.rkt"
         "format.rkt"
         "integrity.rkt"
         "state.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt"
         "printer.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "subprogram.rkt")

(define+provide-message $input (name))
(define+provide-message $input:not-found $input ())
(define+provide-message $input:log $input (messages))

(define current-inputs
  (make-parameter null))

(struct package-input (name plinth))

(define (abstract-package-input? v)
  (and (package-input? v)
       (not (package-input-plinth v))))

(define (make-package-input name [plinth #f])
  (package-input name plinth))

(define (input-ref name)
  (find-input (current-inputs) name))

(define-subprogram (find-input inputs name)
  (for ([i (in-list inputs)])
    (when (equal? name (package-input-name i))
      ($use i)))
  ($fail ($input:not-found name)))

(define (keep-input name)
  (mdo i := (input-ref name)
       (resolve-input i)))

(define-subprogram (release-input input)
  (with-handlers ([exn:fail:filesystem? (λ _ ($use (void)))])
    ($use (delete-file (package-input-name input)))))

(define (fetch-input input)
  (mdo arti           := (find-artifact-for-input input)
       file-record    := (fetch-artifact (package-input-name input) arti)
       (verify-artifact arti file-record) ; Security critical
       (subprogram-unit file-record)))

(define (find-artifact-for-input input)
  (subprogram-combine
   (find-artifact (package-input-plinth input))
   (λ (messages to-wrap)
     (cons ($input:log (package-input-name input)
                       (reverse to-wrap))
           messages))))

(define (resolve-input input)
  (mdo file-record  := (fetch-input input)
       link-name    := (subprogram-unit (package-input-name input))
       (subprogram
        (λ (messages)
          (make-file-or-directory-link
           (find-relative-path
            (current-directory)
            (build-workspace-path (path-record-path file-record)))
           link-name)
          (values link-name
                  messages)))))

(define-source #:key get-untrusted-source-key (untrusted-source [input package-input?])
  (define subprogram
    (mdo record := (fetch-input input)
         (subprogram-unit
          (%fetch (file-source (build-workspace-path (path-record-path record)))))))
  (define-values (result messages) (run-subprogram subprogram null))
  (when (eq? result FAILURE)
    (%fail messages)))

(define (get-untrusted-source-key i)
  (match-define (package-input name plinth) i)
  (input-port-append
   (open-input-string name)
   (if (artifact? plinth)
       (or (identify (artifact-source plinth))
           (open-input-bytes #""))
       (open-input-string
        (with-handlers ([values (λ (e) (open-input-bytes #""))])
          (~s plinth))))))

(module+ test
  (require rackunit)

  (test-case "Detect abstract inputs"
    (check-true (abstract-package-input? (make-package-input "")))
    (check-true (abstract-package-input? (make-package-input "" #f)))
    (check-false (abstract-package-input? (make-package-input "" #t)))))
