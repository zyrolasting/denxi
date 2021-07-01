#lang xiden

(output "default"
        (keep-standalone-racket-module "v1.rkt")
        (keep-standalone-racket-module "v2.rkt")
        (keep-standalone-racket-module "symlinked.rkt"))

(input "v1.rkt" (artifact (file-source (from-file "sources/v1.rkt")) #f))
(input "v2.rkt" (artifact (file-source (from-file "sources/v2.rkt")) #f #f))
(input "symlinked.rkt" (artifact (file-source (from-file "sources/symlinked.rkt")) #f #f))
