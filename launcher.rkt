#lang racket/base

(define-syntax-rule (r s ...)
  (begin (begin (provide (all-from-out s))
                (require s)) ...))

(r racket/base
   "archive.rkt"
   "artifact.rkt"
   "cli-flag.rkt"
   "cli.rkt"
   "cmdline.rkt"
   "codec.rkt"
   "crypto.rkt"
   "dig.rkt"
   "file.rkt"
   "format.rkt"
   "input.rkt"
   "integrity.rkt"
   "l10n.rkt"
   "message.rkt"
   "monad.rkt"
   "notary.rkt"
   "output.rkt"
   "package.rkt"
   "path.rkt"
   "port.rkt"
   "printer.rkt"
   "query.rkt"
   "racket-module.rkt"
   "racket-version.rkt"
   "rfc4648.rkt"
   "security.rkt"
   "setting.rkt"
   "signature.rkt"
   "source.rkt"
   "state.rkt"
   "string.rkt"
   "subprogram.rkt"
   "system.rkt"
   "transaction.rkt"
   "url.rkt"
   "version.rkt")

(module reader syntax/module-reader xiden/launcher)
