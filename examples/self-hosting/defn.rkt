#lang denxi

(os-support unix)

(output "default"
        inp := (input-ref "install-racket.sh")
        path := (resolve-input inp)
        (run "sh" path "--in-place" "--dest" "racket")
        (release-input inp)
        (run "./racket/bin/racket" "-U" "-A" "./addon"
             "-l" "raco/main" "--" "pkg" "install" "-i" "--auto" "denxi"))

(input "install-racket.sh"
       (artifact (http-source "https://download.racket-lang.org/releases/8.1/installers/racket-minimal-8.1-x86_64-linux-cs.sh")
                 (integrity 'sha1
                            (hex "be40b5de3447679f3ad672f46aa2f2784c6002c2"))
                 #f))
