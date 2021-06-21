#lang racket/base

(define-syntax-rule (r s ...)
  (begin (begin (provide (all-from-out s))
                (require s)) ...))

(r racket/base
   "archive.rkt"
   "artifact.rkt"
   "cli.rkt"
   "codec.rkt"
   "crypto.rkt"
   "format.rkt"
   "state.rkt"
   "integrity.rkt"
   "package.rkt"
   "printer.rkt"
   "security.rkt"
   "signature.rkt"
   "source.rkt"
   "subprogram.rkt"
   "system.rkt")

(module reader syntax/module-reader xiden/launcher)
