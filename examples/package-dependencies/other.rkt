#lang xiden

(name "other")

(output "default"
        inp := (input-ref "module.rkt")
        (resolve-input inp))

(input "module.rkt"
       (artifact (text-source
                  "(module anon racket/base (provide msg) (define msg \"It works!\"))")
                 #f
                 #f))
