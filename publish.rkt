#lang racket/base

(require racket/path
         "zcpkg-info.rkt"
         "verify.rkt"
         "archiving.rkt")

(define (prepare-shipment #:private-key [private-key #f]
                          zcpkg-info-file)
  (define output-directory (build-path "for-catalog"))
  (make-directory output-directory)

  (parameterize ([current-directory output-directory])
    (define metadata (read-zcpkg-info zcpkg-info-file))
    (define source-directory (path-only zcpkg-info-file))
    (define archive (pack source-directory))
    (define digest (make-digest archive))

    (define signature
      (and private-key
           (make-signature digest private-key)))

    (define external-zcpkg-info
      (struct-copy zcpkg-info/public
                   metadata
                   [ctime (current-seconds)]
                   [integrity digest]
                   [signature signature]))

    (call-with-output-file
      (build-path output-directory "info.rkt")
      (λ (o) (write-zcpkg-info external-zcpkg-info o)))

    output-directory))
