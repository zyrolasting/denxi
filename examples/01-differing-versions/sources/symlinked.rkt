; Resolves symlinks at compile time.
#lang racket/base

(provide symlinked)

(require (for-syntax racket/base
                     racket/path
                     racket/require-transform
                     syntax/location))

(define-syntax symlinked
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ path-string)
        (expand-import
         (datum->syntax stx
                        `(file
                          ,(path->string (normalize-path (syntax-e #'path-string)
                                                         (or (syntax-source-directory stx)
                                                             (current-directory)))))))]))))
