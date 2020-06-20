#lang racket/base

(provide find-zcpkg-dependency-directory
         resolve-zcpkg-module-path
         DEPENDENCY-DIRNAME
         make-dependency-path
         ../)

(require idiocket/contract
         idiocket/file
         idiocket/function
         idiocket/generator
         idiocket/list
         idiocket/path
         idiocket/string)

(define DEPENDENCY-DIRNAME "zcpkgs")

(define (find-zcpkg-dependency-directory current-dir scoping-path)
  (define candidate
    (simplify-path (build-path (path->complete-path current-dir) DEPENDENCY-DIRNAME scoping-path)))
  (if (directory-exists? candidate)
      candidate
      (and (not (equal? current-dir (../ current-dir)))
           (find-zcpkg-dependency-directory (../ current-dir) scoping-path))))

(define (resolve-zcpkg-module-path module-path [current-dir (current-directory)])
  ; We only want to continue searching for a module if a scoping
  ; directory doesn't exist. If we always continued searching, then it
  ; would be possible for a user to load a module from a package they
  ; were not expecting--Node.js has this problem.
  (define-values (scoping-path scope-relative-path) (split-zcpkg-module-path module-path))
  (define depdir (find-zcpkg-dependency-directory current-dir scoping-path))
  (and depdir
       (for/or ([p (in-candidate-modules scope-relative-path)])
         (get-path-if-file-exists
          (build-path depdir p)))))

(define (split-zcpkg-module-path p)
  (define elements (explode-path p))
  (unless (>= (length elements) 2)
    (raise-argument-error 'zcpkg
                          "A module path including the distributor and package name."
                          p))
  (define-values (head tail) (split-at elements 2))
  (values (apply build-path head)
          (apply build-path (if (null? tail)
                                (list "main")
                                tail))))

(define (in-candidate-modules f)
  (define (yield-by-ext f ext)
    (yield (path-replace-extension f ext))
    (yield (path-replace-extension (build-path f "main") ext)))
  (in-generator
   (yield-by-ext f #".rkt")
   (yield-by-ext f #".ss")))

; Build simplified paths representing parent directories.
;
; ex: (equal? (../ "/home/blah") (build-path "/home/"))
; ex: (equal? (../ "/") (build-path "/"))

(define (../ p)
  (simplify-path (build-path p 'up)))

(define (make-dependency-path distributor-name zcp-name . paths)
  (apply build-path DEPENDENCY-DIRNAME distributor-name zcp-name paths))
