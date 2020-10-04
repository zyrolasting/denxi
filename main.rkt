#lang racket/base
(require xiden/package-definition-module-language)
(provide (all-from-out xiden/package-definition-module-language))
(module reader syntax/module-reader xiden/package-definition-module-language)
