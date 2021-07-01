#lang xiden
(input "maybe" (artifact (http-source "http://localhost:9147") #f #f))
(output "default" (keep-input "maybe"))
