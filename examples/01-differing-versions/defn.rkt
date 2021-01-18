#lang xiden

(output "default"
        (keep-standalone-racket-module "v1.rkt")
        (keep-standalone-racket-module "v2.rkt")
        (keep-standalone-racket-module "symlinked.rkt"))

(input "v1.rkt" (sources (from-file "sources/v1.rkt")))
(input "v2.rkt" (sources (from-file "sources/v2.rkt")))
(input "symlinked.rkt" (sources (from-file "sources/symlinked.rkt")))
