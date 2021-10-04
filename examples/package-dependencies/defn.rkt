#lang denxi

(output "default"
        inp := (input-ref "pkgdef.rkt")
        path := (resolve-input inp)
        (install "nested" "default" path)
        (release-input inp))

(input "pkgdef.rkt"
       (artifact (file-source (from-file "other.rkt"))
                 #f
                 #f))
