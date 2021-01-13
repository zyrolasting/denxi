#lang xiden

(define (get-module name)
  (mdo module-input := (input-ref name)
       (resolve-input module-input)))

(output "default"
        (get-module "v1.rkt")
        (get-module "v2.rkt")
        (get-module "symlinked.rkt"))

(input "v1.rkt" (sources (from-file "sources/v1.rkt")))
(input "v2.rkt" (sources (from-file "sources/v2.rkt")))
(input "symlinked.rkt" (sources (from-file "sources/symlinked.rkt")))
