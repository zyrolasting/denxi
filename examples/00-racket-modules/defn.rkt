#lang xiden

(output "default"
        module-input := (input-ref "hello.rkt")
        (resolve-input module-input))

(input "hello.rkt"
       (sources "https://gist.githubusercontent.com/zyrolasting/545527daa3d650298d27394f737a702f/raw/5363e4a59ccac192855c9b6e1c11020ebedb8e99/hello.rkt")
       (integrity 'sha384 (base64 "12j5qnfZKY57dZWCHxEV9VPbNFMiA7xppf1tW5vU7mlwXiuYCmSyqpJ9xDcOuTaz"))
       (signature "https://sagegerard.com/public.pem"
                  (base64 "bW2jjV+Sn6tpqQ4Dcdaw64q/qQMxGGS0fFk7kXPqSMFajY/4DPDJmWE/vfKlPDPchW16aohT5Tq57PSlHEImnv6Turf1MdZDQXZKtjr+xWc6vyXgG6LAUrgbGiOVbvdSEPFiq+J1FtX7H1cPW7KYQnEieCv0t1f4+jSiifWRcQPGlI9hbXRaX8nYTHFbIyZpen2utmBpsPyBKZ0AIslo+kNtoXD3mLQT/iu3y5O1p6NRQAbAWsBhwD0QR3LqnJO44x/HAkqD4E/09w8J9uzTozpMQvN89FaItdNMtEmQYfnG2GdlLtLaCS43e0WxKoB53XorUnRC1mn0Df6fBsnBuiNhL95cnBLqDsHvryV7Br7U7PhGy12kyhsejrf1F91e0OqeNVZ61+0YmKD+zK+97m7U3NEZDUjR3URRqpzq8CaBAHKoXcDddqjk/mMe6QnMGmUPYpjOqUuuVm6f+RWtGZs9k/Z8tk4biwLNeUWKH+9/bvuZaPHMTDsGWRQ1XwwpYUo2LDG7SKsqEQadqmU263sxl4GJ43S3o4gbcgpMaD6gad6HjexoGEPaoT7/PQNYKEHUbFfBRQ/XgxGUaMDAfRdGjUCoK78agVKUddspKnPO6tyb0E6TTLRTicFIC7eErxmMT3HJQLxkGH7wXIp/oOGUgVwx6wemGpOG1BDKlr8=")))
