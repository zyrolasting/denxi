#lang denxi

; `racket-versions` defines accepted Racket runtime versions when
; using this definition.
;
; To be clear: The restriction applies to the processing of the
; package definition, not what it creates. You can try to have the
; former mean the latter if you are shipping Racket programs, but
; values lower than what Denxi supports will not even make it to the
; check. :)


; Allow any version (no bounds)
; (racket-versions "*")
; (racket-versions ["*" "*"])

; Racket <=5.0 (boundless on left side of interval)
; (racket-versions ["*" "5.0"])

; Racket >=5.0 (right side)
; (racket-versions ["5.0" "*"])

; Exactly 6.7 (interval contains exactly one element)
; (racket-versions "6.7")
; (racket-versions ["6.7" "6.7"])

; (>=5.0 AND <=5.5) OR 5.8
; (racket-versions ["5.0" "5.5"] "5.8"])

;;;;;

; Run `racket -e '(version)'` to see your version, then try installing
; the definition with different values of this.
(racket-versions "7.9" "7.4" ("7.6" "7.7"))

(input "data" (artifact #"_" #f #f))
(output "default" (keep-input "data"))
