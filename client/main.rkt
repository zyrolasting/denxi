#lang racket/base

(require racket/cmdline
         "../config.rkt"
         "new.rkt"
         "config.rkt"
         "capture.rkt"
         "restore.rkt"
         "link.rkt"
         "download.rkt"
         "upload.rkt"
         "install.rkt"
         "uninstall.rkt"
         "show.rkt"
         "recant.rkt"
         "serve.rkt")

(define HELP-FMT #<<EOF
Usage: raco zcpkg <subcommand> [args] ...

  raco zcpkg install:   Install packages
  raco zcpkg uninstall: Uninstall packages
  raco zcpkg new:       Make skeleton package

  raco zcpkg show:      Review key information
  raco zcpkg config:    Set options

  raco zcpkg capture:   Capture workspace
  raco zcpkg restore:   Restore captured workspace

  raco zcpkg serve:     Host a catalog
  raco zcpkg register:  Register an account
  raco zcpkg download:  Download from a catalog
  raco zcpkg upload:    Upload to a catalog
  raco zcpkg recant:    Stop distributing an uploaded artifact

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
                      `((once-each
                         ,(ZCPKG_VERBOSE/make-flag-spec
                           (λ (flag-string help-str param)
                             `((,flag-string)
                               ,(λ _ (param #t))
                               (,help-str))))))
                      (λ (flags action . args)
                        (parameterize ([current-command-line-arguments (list->vector args)])
                          ((match action
                             ["install"   install-command]
                             ["uninstall" uninstall-command]
                             ["new"       new-command]
                             ["serve"     serve-command]
                             ["link"      link-command]
                             ["config"    config-command]
                             ["show"      show-command]
                             ["capture"   capture-command]
                             ["restore"   restore-command]
                             ["download"  download-command]
                             ["upload"    upload-command]
                             ["recant"    recant-command]
                             [_ (λ () (unrecognized-command action))]))))
                      '("subcommand" "args")
                      void))
