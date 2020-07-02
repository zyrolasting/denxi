#lang racket/base

(require racket/cmdline
         "config.rkt"
         "capture.rkt"
         "restore.rkt"
         "download.rkt"
         "upload.rkt"
         "install.rkt"
         "uninstall.rkt"
         "update.rkt"
         "show.rkt"
         "serve.rkt"
         "feedback.rkt")

(define HELP-FMT #<<EOF
Usage: raco zcpkg <subcommand> [args] ...

  raco zcpkg install:   Install packages
  raco zcpkg uninstall: Uninstall packages
  raco zcpkg update:    Change package version

  raco zcpkg show:      Review key information
  raco zcpkg config:    Set options

  raco zcpkg capture:   Capture workspace
  raco zcpkg restore:   Restore captured workspace

  raco zcpkg serve:     Host a catalog
  raco zcpkg register:  Register an account
  raco zcpkg download:  Download from a catalog
  raco zcpkg upload:    Upload to a catalog
  raco zcpkg feedback:  Send feedback

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
                             ["config"    config-command]
                             ["show"      show-command]
                             ["feedback"  feedback-command]
                             ["capture"   capture-command]
                             ["restore"   restore-command]
                             ["download"  download-command]
                             ["upload"    upload-command]
                             [_ (λ () (unrecognized-command action))]))))
                      '("subcommand" "args")
                      void))
