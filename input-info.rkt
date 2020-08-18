#lang racket/base

(provide (struct-out bytes-info)
         get-xiden-object-path)


(require racket/function
         racket/match
         racket/port
         "contract.rkt"
         "encode.rkt"
         "file.rkt"
         "integrity.rkt"
         "signature.rkt"
         "source.rkt"
         "workspace.rkt"
         "xiden-messages.rkt")


(struct bytes-info
  (usage      ; The intended use of the bytes
   name       ; The name of the file to hold the input bytes
   sources    ; Where to look to get the input bytes
   integrity  ; Integrity information to make sure we got the right bits
   signature) ; Signature information to make sure we got the bits from the right party
  #:prefab)


(define (get-xiden-object-path digest)
  (define full
    (bytes->string/utf-8
     (encode 'base32 digest)))
  (build-workspace-path
   "var/xiden/objects"
   (substring full 0
              (min (string-length full) 64))))


(define (make-xiden-object-file binfo)
  (with-handlers
    ([$bad-digest?
      (λ (m) (make-xiden-object-file
              (struct-copy bytes-info binfo
                           [sources (cdr (bytes-info-sources binfo))])))])
    (call-with-first-source
     (bytes-info-sources binfo)
     (λ (in)
       ; Copy the source to the filesystem immediately.
       ; We don't know how big it is, so we don't want
       ; to buffer the entire port's contents in memory.
       (define tmp (make-temporary-file))
       (call-with-output-file tmp #:exists 'truncate/replace
         (λ (to-file) (copy-port in to-file)))

       (case (check-integrity (bytes-info-integrity binfo) tmp)
         [(missing) (raise ($missing-digest binfo))]
         [(mismatch) (raise ($bad-digest binfo))])

       ;(delete-file tmp)

       (rename-file-or-directory tmp (get-xiden-object-path (make-digest tmp)))))))
