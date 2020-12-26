#lang racket/base
(require xiden/pkgdef)
(provide (all-from-out xiden/pkgdef))
(module reader syntax/module-reader xiden/pkgdef)
