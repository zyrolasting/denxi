#lang racket/base

; Pack and unpack archives containing packages.
; Hide the algorithms responsible from the user.

(require racket/contract)

(provide (contract-out [pack   (->* (directory-exists?) (#:to path-string?) path?)]
                       [unpack (->* (archive-path?) (#:to path-string?) path?)]
                       [archive-path? predicate/c]))

(require "file.rkt"
         racket/path
         file/tar
         file/gzip
         file/untgz)

(define (pack dir #:to [dest (path-only dir)])
  (define archive-path (build-path dest (make-archive-name dir)))
  (parameterize ([current-directory dir])
    (apply tar-gzip
           archive-path
           (find-archive-files)
           #:path-prefix (file-name-from-path dir)
           #:follow-links? #f
           #:exists-ok? #t)
    archive-path))

(define (unpack path #:to [dest (path-only path)])
  (untgz path #:dest dest)
  dest)

(define (archive-path? path)
  (equal? (path-get-extension path) #".tgz"))

(define (make-archive-name dir)
  (path-replace-extension (file-name-from-path dir)
                          #".tgz"))

(define (find-archive-files)
  (map (λ (p) (maybe-complete-path->relative-path p))
       (find-files for-archive? #:skip-filtered-directory? #f)))

(define (for-archive? p)
  (and (not (link-exists? p))
       (file-exists? p)
       (not (member (path->string (file-name-from-path p))
                    '(".git" ".travis.yml" ".gitignore" "compiled" "doc")))))

(define (maybe-complete-path->relative-path p)
  (if (complete-path? p)
      (find-relative-path (current-directory) p)
      p))

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
                      (define .tgz (pack "input-dir"))
                      (test-pred "Archive exists" file-exists? .tgz)
                      (test-pred "Archive path is recognizeable" archive-path? .tgz)

                      (delete-directory/files "input-dir")
                      (unpack .tgz)

                      (test-file input-a)
                      (test-file input-b)
                      (test-file input-c)))
                  (λ () (delete-directory/files working-dir)))))
