#lang racket/base

(provide compile-installed)

(require racket/function
         racket/match
         racket/path
         setup/parallel-build
         "config.rkt"
         "file.rkt"
         "zcpkg-info.rkt"
         "zcpkg-settings.rkt")

(define (compile-installed [compile? (const #t)])
  (for ([info (in-installed-info)])
    (when (compile? info)
      (compile-zcpkg-files (zcpkg-info->install-path info)))))

(define (compile-zcpkg-files install-path)
  (parallel-compile-files
   (find-files (λ (p) (and (member (path-get-extension p)
                                   '(#".rkt" #".ss" #".scrbl"))
                           #t))
               install-path)
   #:handler (λ (type work msg out err)
               (match type
                 ['done (when (ZCPKG_VERBOSE) (printf " Made ~a\n" work))]
                 ['output (printf " Output from: ~a\n~a~a" work out err)]
                 [else (printf " Error compiling ~a\n~a\n~a~a"
                               work
                               msg
                               out
                               err)]))))
