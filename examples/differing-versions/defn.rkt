#lang xiden

(name "example01-output")

(output "default"
        (keep-standalone-racket-module "v1.rkt")
        (keep-standalone-racket-module "v2.rkt")
        (keep-standalone-racket-module "symlinked.rkt"))

(input "v1.rkt" (artifact (sources (from-file "sources/v1.rkt"))))
(input "v2.rkt" (artifact (sources (from-file "sources/v2.rkt"))))
(input "symlinked.rkt" (artifact (sources (from-file "sources/symlinked.rkt"))))
