#lang racket/base

(require racket/contract)
(provide (struct-out package-input)
         (struct-out untrusted-source)
         (struct-out $input)
         (struct-out $input:not-found)
         (struct-out $input:abstract)
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
         "message.rkt"
         "monad.rkt"
         "port.rkt"
         "signature.rkt"
         "source.rkt"
         "string.rkt"
         "subprogram.rkt")

(define-message $input (name))
(define-message $input:not-found $input ())
(define-message $input:abstract $input ())

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

(define (resolve-input input)
  (define name (package-input-name input))
  (define plinth (package-input-plinth input))
  (if (artifact? plinth)
      (mdo known := (study-artifact (package-input-name input))
           names := (known-get-names known)
           (known-put-names known (cons name names)))
      (subprogram-failure ($input:abstract name))))



(module+ test
  (require rackunit)

  (test-case "Detect abstract inputs"
    (check-true (abstract-package-input? (make-package-input "")))
    (check-true (abstract-package-input? (make-package-input "" #f)))
    (check-false (abstract-package-input? (make-package-input "" #t)))))
