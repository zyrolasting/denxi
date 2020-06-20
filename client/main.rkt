#lang racket/base

(require racket/cmdline
         "download.rkt"
         "install.rkt"
         "uninstall.rkt"
         "update.rkt"
         "show.rkt"
         "feedback.rkt"
         "lock.rkt")

(define HELP-FMT #<<EOF
Usage: raco zcpkg <subcommand> [args] ...

Package management:
  raco zcpkg install:   Install packages
  raco zcpkg uninstall: Uninstall packages
  raco zcpkg update:    Change package version
  raco zcpkg show:      Describe packages
  raco zcpkg lock:      Record expected dependencies in lock file
  raco zcpkg resolve:   Make a concrete dependency name

Catalog:
  raco zcpkg register:  Register a catalog
  raco zcpkg login:     Authenticate against a catalog
  raco zcpkg download:  Download a package artifact
  raco zcpkg publish:   Submit a package to a catalog

Other:
  raco zcpkg feedback:  Send feedback
  raco zcpkg config:    Set options

EOF
)


(define (show-help)
  (displayln HELP-FMT))

(define (unrecognized-command action)
  (printf "Unrecognized command: ~a~n~n" action)
  (show-help)
  (exit 1))

(module+ main
  (require racket/match)
  (parse-command-line "zcpkg" (current-command-line-arguments)
                      '()
                      (λ (flags action . args)
                        (parameterize ([current-command-line-arguments (list->vector args)])
                          ((match action
                             ["install"   install-command]
                             ["uninstall" uninstall-command]
                             ["update"    update-command]
                             ["show"      show-command]
                             ["feedback"  feedback-command]
                             ["lock"      lock-command]
                             ["download"  download-command]
                             [_ (λ () (unrecognized-command action))]))))
                      '("subcommand" "args")
                      void))
