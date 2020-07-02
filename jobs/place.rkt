#lang racket/base

; Define an entry point for paralell work.

(provide main)

(require racket/function
         racket/match
         racket/exn
         "messages.rkt"
         "../operations.rkt"
         "../printer.rkt")

(define (main pch)
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (parameterize ([current-place-channel pch])
      (let loop ()
        (define msg (sync/enable-break pch))
        (parameterize ([current-job-id ($run-id msg)])
          (with-handlers ([exn? (λ (v) (<< "~s" (exn->string v)) (exit 1))])
            (run ($run-command msg))
            (<<fin)
            (loop)))))))

(define (run command)
  (match command
    [(vector "install" package-source)
     (install package-source)]
    [(vector "update" package-path package-source)
     (uninstall package-path)
     (install package-source)]
    [(vector "uninstall" package-path)
     (uninstall package-path)]
    [v (<< "Unsupported value: ~s~n" v)]))
