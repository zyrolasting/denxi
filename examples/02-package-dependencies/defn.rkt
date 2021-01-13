#lang xiden

(output "default"
        inp := (input-ref "pkgdef.rkt")
        path := (resolve-input inp)
        (install "nested" "default" path)
        (release-input inp))

(input "pkgdef.rkt"
       (sources (from-file "other.rkt")))
