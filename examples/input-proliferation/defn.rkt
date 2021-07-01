#lang xiden

; Xiden assumes that outputs with different names necessarily produce
; different content. This definition will therefore create duplicate
; data despite starting from the same input. This will install without
; issue, but you should not create equivalent outputs.

; A G-zipped TAR in byte string form.
(input "archive.tgz"
       (artifact (byte-source #"\37\213\b\0\0\0\0\0\0\3\355\321Q\n\2!\20\6`\217\342\21\224\\=\217E\364\336n\367\317\36\202\236ZXH\b\276\17\206\1\35\230\201\277\207\337KC\255\345\325s[\322g\177\v\271\244\332rK\243\306{\253\345\24b\232p[x\254[\277\307\30\326~\273~\233\333\373\377S\347\t;\16\345_\344?\303e\302\216C\371/\362\a\0\0\0\0\0\0\0\200=O\177\203}\e\0(\0\0")
                 #f
                 #f))

(output "tweedledee" (extract-input "archive.tgz"))
(output "tweedledum" (extract-input "archive.tgz"))
