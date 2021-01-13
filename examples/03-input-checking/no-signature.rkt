#lang xiden

(output "default"
        inp := (input-ref "data")
        (resolve-input inp))

(input "data"
       (sources (from-file "code.txt"))
       (integrity 'sha384 (base64 "qAhYrIsnCp+iSA1sPg74sfLbv/PsRSUVL0K6krxwRAwvdOEVxloL089YFXw1xukS")))
