#lang racket/base

(require "contract.rkt")

(provide (struct-out signature-info)
         (contract-out
          [well-formed-signature-info/c
           flat-contract?]
          [check-signature
           (-> bytes?
               (or/c path-string? bytes?)
               (or/c path-string? bytes?)
               boolean?)]))


(require racket/sequence
         racket/format
         racket/port
         "encode.rkt"
         "file.rkt"
         "integrity.rkt"
         "openssl.rkt"
         "rc.rkt"
         "url.rkt")

(struct signature-info (pubkey body) #:prefab)


(define (well-formed-signature-info/c info)
  (struct/c signature-info
            (or/c bytes? string?)
            (or/c bytes? string?)))


(define ESTIMATED_SIGNATURE_AND_PUBKEY_MAX_SIZE (* 100 1024))

(define (check-signature digest public-key-variant signature-variant)
  (regexp-match?
   #rx#"Success"
   (run-openssl-command (open-input-bytes digest)
                        "pkeyutl"
                        "-verify"
                        "-sigfile" (get-cached-file* signature-variant ESTIMATED_SIGNATURE_AND_PUBKEY_MAX_SIZE)
                        "-pubin"
                        "-inkey" (get-cached-file* public-key-variant ESTIMATED_SIGNATURE_AND_PUBKEY_MAX_SIZE))))
