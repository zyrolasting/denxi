#lang xiden
(name "my-pkg")
(input "B" (artifact (text-source "B\n") #f #f))
(output "default" (keep-input "B"))
