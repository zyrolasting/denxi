#lang xiden

(name "example02-output")

(output "default"
        inp := (input-ref "pkgdef.rkt")
        path := (resolve-input inp)
        (install "nested" "default" path)
        (release-input inp))

(input "pkgdef.rkt"
       (artifact (sources (from-file "other.rkt"))))
