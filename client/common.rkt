#lang racket/base

(provide act-on-next-argument
         (all-from-out racket/cmdline
                       racket/match
                       "../workspace.rkt"
                       "../config.rkt"))

(require racket/cmdline
         racket/match
         "../workspace.rkt"
         "../config.rkt")

(define-syntax-rule (act-on-next-argument name show-help [str fn] ...)
  (parse-command-line name (current-command-line-arguments)
                      '()
                      (λ (flags action . args)
                        (parameterize ([current-command-line-arguments (list->vector args)])
                          ((match action
                             [str fn] ...
                             [_ show-help]))))
                      '("action")
                      show-help))
