#lang xiden
(name "my-pkg")
(input "A" (artifact (text-source "A\n") #f #f))
(output "default" (keep-input "A"))
