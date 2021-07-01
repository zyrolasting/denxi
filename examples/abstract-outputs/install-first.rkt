#lang xiden
(input "data" (artifact (byte-source #"_\n") #f #f))
(output "default" (keep-input "data"))
