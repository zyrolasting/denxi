#lang racket/base

; Define a package input format and related operations.

(require racket/contract)
(provide (struct-out input-info)
         (contract-out
          [well-formed-input-info/c
           flat-contract?]
          [fulfill-input
           (-> well-formed-input-info/c
               (-> well-formed-input-info/c string? (or/c #f complete-path?))
               complete-path?)]
          [build-input-path
           (-> bytes? complete-path?)]))


(require "encode.rkt"
         "file.rkt"
         "integrity.rkt"
         "rc.rkt"
         "signature.rkt"
         "string.rkt"
         "workspace.rkt")


(struct input-info
  (name       ; The name of the link used to reference input bytes
   sources    ; Where to look to get bytes
   integrity  ; Integrity information: Did I get the right bytes?
   signature) ; Signature for authentication: Did the bytes come from someone I trust?
  #:prefab)


(define well-formed-input-info/c
  (struct/c input-info
            non-empty-string?
            (non-empty-listof any/c)
            (or/c (λ (info) (XIDEN_TRUST_BAD_DIGEST))
                  well-formed-integrity-info/c)
            (or/c (λ (info) (or (XIDEN_TRUST_BAD_DIGEST)
                                (XIDEN_TRUST_UNSIGNED)))
                  well-formed-signature-info/c)))


(define (fulfill-input input fetch)
  (for/fold ([path (get-maybe-existing-input-path input)])
            ([source (in-list (input-info-sources input))])
    (or path
        (fetch input source))))


(define (build-input-path digest)
  (build-workspace-path
   "var/xiden/input"
   (encoded-file-name digest)))


(define (get-maybe-existing-input-path input)
  (with-handlers ([exn? (λ _ #f)])
    (define path
      (build-input-path
       (integrity-info-digest
        (input-info-integrity input))))
    (and (file-exists? path)
         path)))
