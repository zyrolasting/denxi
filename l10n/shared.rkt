#lang racket/base

(define-syntax-rule (r s ...)
  (begin (begin (provide (all-from-out s))
                (require s)) ...))

(r "../cli-flag.rkt"
   "../cmdline.rkt"
   "../exn.rkt"
   "../format.rkt"
   "../input-info.rkt"
   "../localstate.rkt"
   "../package.rkt"
   "../printer.rkt"
   "../port.rkt"
   "../racket-version.rkt"
   "../rc.rkt"
   "../sentry.rkt"
   "../signature.rkt"
   "../source.rkt"
   "../string.rkt"
   "../worker.rkt"
   "../workspace.rkt")
