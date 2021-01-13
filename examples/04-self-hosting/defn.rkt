#lang xiden

(os-support unix)

(output "default"
        inp := (input-ref "install-racket.sh")
        path := (resolve-input inp)
        (run "sh" path "--in-place" "--dest" "racket")
        (release-input inp)
        (run "./racket/bin/racket" "-U" "-A" "./addon"
             "-l" "raco/main" "--" "pkg" "install" "-i" "--auto" "xiden"))

(input "install-racket.sh"
       (http-source "https://mirror.racket-lang.org/installers/7.9/racket-minimal-7.9-x86_64-linux.sh")
       (integrity 'sha384 (base64 "fsVS1vWPBnSkv0MRjQtH6/ES/JqIXxi+JrslIN/rKGsgC1yVeXT1x4ofE4udUoE+"))
       (signature "https://sagegerard.com/public.pem"
                  (base64 "BZ82a5GuaW0LVLByuvkudrQJOymutdlUU5tpIVo9cFuVjJU8+91oTAB9p6CDcWmQbV12QS7W7ujePTalSaLXLVYWgO7DVukDywkQ81e0a4686xKRFL4tDSWAdHPHkf2zqG8YCHZAbauDoayACMtEVnaJp7bzhOGTbwW5kEVZ/k9jGytAE31LVZS2abj4YRV7cswQlzzyp0Zgek4tiRAYiYgzOAMWqMYIs+HFuJECF1T99oVUpNl/p49ECXyRlj3VUSOB0LGBlPSI8ntga+0FCUM5HJzpzQLFncG6dPbMH9x+5I6eTPW2NUa7+pkTFTdoeSkae5SuIkDpE1qS2Rr9suORnzX2a9A1LKkeqMpdzen6hD+GwA5ksCSCCZx3+RNAHL5Dg3Mfe+39W9mpCmAiqm7GoGOHyNhUWv13biYE6tLOFadNr38MSCBGwtRKb9ALaTtO9Pityn6Tuh7JS5qtHzcc9zEpIEY7soebNPfhi10prAg9cADSiII3zVvMo+WjdbmlNBBL58OzDs4WFN4AdoJP5u9IxazWC3yQuH2KISPj6AtO7khN5t5Sr0aHgGnuNqCp+2xK3F0ZhOh7q6UdL+2jCm/vL1W4jySZs8yxAm9XLxjEm7DIVICLpekaYKJNFqe2ViDLQidIT2fj+2Hu1JapSB+KcEXNBWyM3pg0dEE=")))
