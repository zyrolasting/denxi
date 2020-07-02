#lang racket/base

(provide show-command)

(require racket/cmdline
         racket/match
         racket/pretty
         "../workspace.rkt")


(define HELP-FMT #<<EOF
Usage: raco zcpkg show <what>

Show a report on requested information.

raco zcpkg show installed: Show installed packages
raco zcpkg show workspace: Show detected workspace path
EOF
)


(define (show-help . _)
  (displayln HELP-FMT))

(define (show-workspace)
  (displayln (ZCPKG_WORKSPACE)))

(define (show-installed)
  (void))

(define (show-command)
  (parse-command-line "zcpkg" (current-command-line-arguments)
                      '()
                      (λ (flags action . args)
                        ((match action
                           ["workspace" show-workspace]
                           ["installed" show-installed]
                           [_ show-help])))
                      '("what")
                      show-help))
