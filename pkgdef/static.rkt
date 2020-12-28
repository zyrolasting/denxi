#lang racket/base

; Operate on package definitions as a syntax objects or lists.

(require "../contract.rkt")

(provide (contract-out
          [PACKAGE_DEFINITION_MODULE_LANG symbol?]
          [PACKAGE_DEFINITION_READER_LANG symbol?]
          [package-definition-datum? predicate/c]
          [get-package-definition-body
           (-> package-definition-datum? list?)]
          [make-package-definition-datum
           (->* (list?) (#:id symbol?) package-definition-datum?)]
          [read-package-definition
           (->* (racket-module-input-variant/c)
                (logged/c syntax?))]
          [make-input-expression-from-files
           (->* (path-string?
                 (-> bytes? path-string? (non-empty-listof any/c))
                 string?
                 path-string?)
                (#:local-name string?
                 #:md-algorithm md-algorithm/c
                 #:byte-encoding (or/c #f xiden-encoding/c)
                 (or/c #f path-string?))
                list?)]))


(require (only-in racket/format ~s)
         "../codec.rkt"
         "../exn.rkt"
         "../integrity.rkt"
         "../logged.rkt"
         "../path.rkt"
         "../racket-module.rkt"
         "../signature.rkt"
         "../string.rkt")

(define PACKAGE_DEFINITION_MODULE_LANG 'xiden/pkgdef)
(define PACKAGE_DEFINITION_READER_LANG 'xiden)

(define (package-definition-datum? v)
  (racket-module-code? PACKAGE_DEFINITION_MODULE_LANG v))

(define (read-package-definition variant)
  (read-racket-module PACKAGE_DEFINITION_READER_LANG PACKAGE_DEFINITION_MODULE_LANG variant))

(define (make-package-definition-datum #:id [id 'pkgdef] body)
  (make-racket-module-datum #:id id PACKAGE_DEFINITION_MODULE_LANG body))

(define (get-package-definition-body datum)
  (get-racket-module-body PACKAGE_DEFINITION_MODULE_LANG datum))


;-------------------------------------------------------------------------------
; Authoring aids

(define (make-input-expression-from-files
         path
         #:local-name [local-name (path->string (file-name-from-path path))]
         #:byte-encoding [byte-encoding 'base64]
         #:md-algorithm [message-digest-algorithm 'sha384]
         make-sources
         public-key-source
         private-key-path
         [private-key-password-path #f])
  (let ([digest (make-digest (expand-user-path path) message-digest-algorithm)]
        [make-byte-expression
         (if byte-encoding
             (λ (bstr) `(,byte-encoding ,(coerce-string (encode byte-encoding bstr))))
             values)])
    `(input ,local-name
            (sources . ,(make-sources digest path))
            (integrity ',message-digest-algorithm ,(make-byte-expression digest))
            (signature ,public-key-source
                       ,(make-byte-expression
                         (make-signature-bytes
                          digest
                          (expand-user-path private-key-path)
                          (and private-key-password-path
                               (expand-user-path private-key-password-path))))))))
