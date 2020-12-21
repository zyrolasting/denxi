#lang racket/base

; The rcfile language is a superset of #lang info and setup/infotab

(require (for-syntax (except-in racket/base require)
                     syntax/strip-context)
         setup/infotab
         "codec.rkt"
         "integrity.rkt")

(provide integrity
         base64
         base32
         hex
         #%top-interaction
         (all-from-out setup/infotab))

(module reader syntax/module-reader xiden/rctab)
