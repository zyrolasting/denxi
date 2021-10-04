#lang denxi

; "Abstract inputs" are inputs without names. They depend on a
; launcher for elaboration.
(input "data")
(output "default" (keep-input "data"))
