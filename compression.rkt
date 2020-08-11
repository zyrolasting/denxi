#lang racket/base

; Define an interface to (de)compress bytes, hiding the algorithm at work.

(require racket/contract
         file/gzip
         file/gunzip)

(provide (contract-out [compress   (-> path-string? (listof path-string?) path-string?)]
                       [decompress (-> path-string? path-string? path-string?)])
         (struct-out exn:fail:zcpkg:decompression))

(struct exn:fail:zcpkg:decompression exn:fail (pos))

(define (compress in out [timestamp (current-seconds)])
  (gzip-through-ports in out #f timestamp))

(define (decompress in out [name "data"])
  (port-count-lines! in)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (raise (exn:fail:zcpkg:decompression
                             (format "Could not decompress ~a: ~a"
                                     name
                                     (exn-message e))
                             (exn-continuation-marks e)
                             (let-values ([(line col pos) (port-next-location in)])
                               pos))))])
    (gunzip-through-ports in out)))

(module+ test
  (require rackunit)

  (define original #"a { marvelous { example { of { redundant { and { superfluous { information } } } } } } }")

  (test-case "Confirm decompression is inverse of compression"
    (define in (open-input-bytes original))
    (define compressed-out (open-output-bytes))
    (define decompressed-out (open-output-bytes))

    (compress in compressed-out)
    (decompress (open-input-bytes (get-output-bytes compressed-out))
                decompressed-out)

    (check-equal? (get-output-bytes decompressed-out #t)
                  original))

  (test-exn "Detect corruption"
    (λ (e)
      (and (exn:fail:zcpkg:decompression? e)
           (eq? (exn:fail:zcpkg:decompression-pos e)
                3)))
    (λ ()
      (define in (open-input-bytes original))
      (define compressed-out (open-output-bytes))

      (compress in compressed-out)

      (define to-corrupt (get-output-bytes compressed-out #t))

      (bytes-set! to-corrupt 0 0)
      (decompress (open-input-bytes to-corrupt) (open-output-bytes)))))
