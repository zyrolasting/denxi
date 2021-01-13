#lang xiden

(output "default"
        inp := (input-ref "data")
        (resolve-input inp))

(input "data"
       (sources (from-file "code.txt")))
