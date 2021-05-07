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
               (logged/c path-string?))]
          [release-input
           (-> package-input?
               (logged/c void?))]
          [keep-input
           (-> string?
               (logged/c path?))]
          [find-input
           (-> (listof package-input?)
               path-string?
               (logged/c package-input?))]
          [input-ref
           (-> string?
               (logged/c package-input?))]
          [current-inputs
           (parameter/c (listof package-input?))]))

(require racket/match
         "artifact.rkt"
         "dig.rkt"
         "format.rkt"
         "integrity.rkt"
         "localstate.rkt"
         "logged.rkt"
         "message.rkt"
         "monad.rkt"
         "port.rkt"
         "printer.rkt"
         "query.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt")

(define+provide-message $input (name))
(define+provide-message $input:not-found $input ())

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

(define-logged (find-input inputs name)
  (for ([i (in-list inputs)])
    (when (equal? name (package-input-name i))
      ($use i)))
  ($fail ($input:not-found name)))

(define (keep-input name)
  (mdo i := (input-ref name)
       (resolve-input i)))

(define-logged (release-input input)
  (with-handlers ([exn:fail:filesystem? (λ _ ($use (void)))])
    ($use (delete-file (package-input-name input)))))

(define (fetch-input input)
  (mdo arti           := (find-artifact (package-input-plinth input))
       file-record    := (fetch-artifact (package-input-name input) arti)
       (verify-artifact arti file-record) ; Security critical
       (logged-unit file-record)))

(define (resolve-input input)
  (mdo file-record    := (fetch-input input)
       link-name      := (logged-unit (package-input-name input))
       link-record    := (logged-unit (make-addressable-link file-record link-name))
       (logged-unit link-name)))

(define-source #:key get-untrusted-source-key (untrusted-source [input package-input?])
  (define subprogram
    (mdo record := (fetch-input input)
         (logged-unit
          (%fetch (file-source (build-workspace-path (path-record-path record)))))))
  (define-values (result messages) (run-log subprogram null))
  (when (eq? result FAILURE)
    (%fail messages)))

(define (get-untrusted-source-key i)
  (match-define (package-input name plinth) i)
  (input-port-append
   (open-input-string name)
   (if (artifact-info? plinth)
       (or (identify (artifact-info-source plinth))
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
