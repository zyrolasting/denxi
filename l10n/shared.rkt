#lang racket/base

(define-syntax-rule (r s ...)
  (begin (begin (provide (all-from-out s))
                (require s)) ...))

(r "../artifact.rkt"
   "../archive.rkt"
   "../cli-flag.rkt"
   "../cmdline.rkt"
   "../dig.rkt"
   "../codec.rkt"
   "../exn.rkt"
   "../file.rkt"
   "../format.rkt"
   "../input.rkt"
   "../integrity.rkt"
   "../localstate.rkt"
   "../logged.rkt"
   "../message.rkt"
   "../openssl.rkt"
   "../package.rkt"
   "../printer.rkt"
   "../port.rkt"
   "../query.rkt"
   "../racket-module.rkt"
   "../racket-version.rkt"
   "../security.rkt"
   "../setting.rkt"
   "../signature.rkt"
   "../source.rkt"
   "../string.rkt"
   "../system.rkt"
   "../url.rkt")
