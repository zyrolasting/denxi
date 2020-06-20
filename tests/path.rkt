#lang racket/base

(require racket/file
         racket/path
         racket/runtime-path
         zcpkg/path
         rackunit)

(define deps/ make-dependency-path)

(define (touch-dep rel-path)
  (define path (build-path (current-directory) DEPENDENCY-DIRNAME rel-path))
  (make-directory* (path-only path))
  (display-to-file #:exists 'truncate/replace
                   "#lang zcpkg/tests/require/lang\n" path)
  path)

(define (test-resolve-zcpkg-module-path req-path expected [cwd (current-directory)])
  (parameterize ([current-directory cwd])
    (test-equal? (format "resolve: ~a" req-path)
                 (resolve-zcpkg-module-path req-path cwd)
                 expected)))

(define (mkdirp . args)
  (make-directory* (apply build-path args)))

(module+ test
  (define tmp-dir (make-temporary-file "~a" 'directory))

  (test-true "Can detect root paths"
             (andmap (λ (root) (equal? root (../ root)))
                     (filesystem-root-list)))

  (parameterize ([current-directory tmp-dir])
    (define acme-path "acme/anvil/drop.rkt")
    (define disney-path "disney/legal/sue.rkt")
    (define shadowing-path "disney/legal/bribe.rkt")

    (define drop.rkt (touch-dep acme-path))
    (define sue.rkt (touch-dep disney-path))

    (test-case "resolve-zcpkg-module-path"
      (test-case "Find module from incomplete relative path."
        (test-resolve-zcpkg-module-path acme-path drop.rkt)
        (test-resolve-zcpkg-module-path disney-path sue.rkt))
      (parameterize ([current-directory (build-path tmp-dir "acme/anvil")])
        (test-case "Shadow all modules from another package of the same name and publisher."
          (define bribe.rkt (touch-dep shadowing-path))
          (test-resolve-zcpkg-module-path shadowing-path bribe.rkt)
          (test-resolve-zcpkg-module-path disney-path #f)))))

  (delete-directory/files tmp-dir))
