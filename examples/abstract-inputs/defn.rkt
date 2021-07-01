#lang xiden

; "Abstract inputs" are inputs without names. They depend on a
; launcher to define them.
(input "data")
(output "default" (keep-input "data"))
