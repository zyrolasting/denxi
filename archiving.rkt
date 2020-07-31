#lang racket/base

; Pack and unpack archives containing packages.
; Hide the algorithms responsible from the user.

(require racket/contract)

(provide (contract-out [pack   (-> path-string? (listof path-string?) path-string?)]
                       [unpack (-> path-string? path-string? path-string?)]))

(require "file.rkt"
         racket/path
         file/tar
         file/gzip
         file/untgz)

(define (pack dest paths)
  (apply tar-gzip
         dest
         paths
         #:follow-links? #f
         #:exists-ok? #t)
  dest)

(define (unpack path [dest (path-only path)])
  (untgz path #:dest dest)
  dest)

(module+ test
  (require rackunit)

  (define (make-file path)
    (make-directory* (path-only path))
    (display-to-file "data" path)
    path)

  (define (test-file path)
    (test-equal? (format "~a restored" path)
                 (file->string path)
                 "data"))

  (test-case "Can pack and unpack an archive"
    (define working-dir (make-temporary-file "tmp~a" 'directory))

    (dynamic-wind void
                  (λ ()
                    (parameterize ([current-directory working-dir])
                      (make-directory "input-dir")
                      (define input-a (make-file "input-dir/a"))
                      (define input-b (make-file "input-dir/sub/b"))
                      (define input-c (make-file "input-dir/very/deep/c"))
                      (define .tgz (pack "a.tgz" (list "input-dir")))
                      (test-pred "Archive exists" file-exists? .tgz)

                      (delete-directory/files "input-dir")
                      (unpack .tgz)

                      (test-file input-a)
                      (test-file input-b)
                      (test-file input-c)))
                  (λ () (delete-directory/files working-dir)))))
