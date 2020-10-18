#lang racket/base

(define-syntax-rule (r s ...)
  (begin (begin (provide (all-from-out s))
                (require s)) ...))

(r "../cli-flag.rkt"
   "../cmdline.rkt"
   "../codec.rkt"
   "../exn.rkt"
   "../format.rkt"
   "../input-info.rkt"
   "../integrity.rkt"
   "../localstate.rkt"
   "../message.rkt"
   "../package.rkt"
   "../printer.rkt"
   "../port.rkt"
   "../racket-version.rkt"
   "../rc.rkt"
   "../setting.rkt"
   "../signature.rkt"
   "../source.rkt"
   "../string.rkt"
   "../workspace.rkt")
