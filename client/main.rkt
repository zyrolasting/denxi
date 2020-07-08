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
Usage: raco zcpkg <subcommand> ...

Where <subcommand> is one of:

  install    Install packages
  uninstall  Uninstall packages
  link       Make links to installed packages
  show       Print helpful reports
  config     Set options
  capture    Capture workspace
  restore    Restore workspace
  diff       Compare workspace to capture
  new        Make a new package
  register   Register an account on a catalog
  recant     Stop distributing an uploaded artifact
  serve      Serve installed packages
  download   Download from a catalog
  pack       Bundle package for upload
  upload     Upload to a catalog
  unpack     Extract bundled package
  run        Run package command

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
