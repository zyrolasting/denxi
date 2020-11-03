#lang racket/base
; Extend reader to use package definition module language.
(require xiden/pkgdef)
(provide (all-from-out xiden/pkgdef))
(module reader syntax/module-reader xiden/pkgdef)
