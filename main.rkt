#lang racket/base
(require denxi/pkgdef)
(provide (all-from-out denxi/pkgdef))
(module reader syntax/module-reader denxi/pkgdef)
