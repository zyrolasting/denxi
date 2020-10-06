#lang racket/base

; Pack and unpack archives containing packages.
; Hide the algorithms responsible from the user.

(require racket/contract)

(provide (contract-out [pack   (-> (listof path-string?) output-port? any)]
                       [unpack (-> (or/c path-string? input-port?) void?)]))

(require "file.rkt"
         racket/path
         file/tar
         file/untar
         file/untgz
         file/unzip)

(define (pack paths out)
  (tar->output paths out #:follow-links? #f))

(define (unpack in)
  (if (path-string? in)
      (call-with-input-file in unpack)
      (void ((get-extract-procedure (object-name in)) in))))

(define (get-extract-procedure name)
  (if (path-string? name)
      (get-extract-procedure (path-get-extension name))
      (case name
        [(#".tar") untar]
        [(#".tgz") untgz]
        [(#".zip") unzip]
        [else (error 'unpack
                     "Cannot infer archive format: ~a"
                     name)])))

(module+ test
  (require rackunit)

  (define (make-file path-string)
    (define path (string->path path-string))
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
                      (define dir/ (build-path "input-dir"))
                      (make-directory dir/)
                      (define input-a (make-file "input-dir/a"))
                      (define input-b (make-file "input-dir/sub/b"))
                      (define input-c (make-file "input-dir/very/deep/c"))
                      (define .tar "a.tar")

                      (call-with-output-file .tar
                        (λ (o) (pack (list input-a input-b input-c) o)))

                      (test-pred "Archive exists" file-exists? .tar)

                      (delete-directory/files dir/)

                      (call-with-input-file .tar
                        (λ (i) (unpack i)))

                      (test-file input-a)
                      (test-file input-b)
                      (test-file input-c)))
                  (λ () (delete-directory/files working-dir)))))
