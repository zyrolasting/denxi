#lang racket/base

; Intentionally leak a PEM-encoded RSA keypair for tests.

(require racket/contract)
(provide
 (contract-out
  [snake-oil-private-key bytes?]
  [snake-oil-public-key bytes?]
  [snake-oil-private-key-password bytes?]))

; usage: racket -l denxi/signature/snake-oil
(module+ main
  (require racket/port)
  (define (<< path bstr)
    (call-with-output-file path (λ (out) (copy-port (open-input-bytes bstr) out))))
  (<< "LEAKED-private-key-password.txt" snake-oil-private-key-password)
  (<< "LEAKED-private-key.pem" snake-oil-private-key)
  (<< "public-key.pem" snake-oil-public-key))

(define snake-oil-private-key-password #"foobar")

(define snake-oil-private-key (string->bytes/utf-8 #<<EOF
-----BEGIN RSA PRIVATE KEY-----
Proc-Type: 4,ENCRYPTED
DEK-Info: AES-128-CBC,E1A24462AABA790A82015D69D2548D52

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
-----END RSA PRIVATE KEY-----
EOF
))


(define snake-oil-public-key (string->bytes/utf-8 #<<EOF
-----BEGIN PUBLIC KEY-----
MIIBojANBgkqhkiG9w0BAQEFAAOCAY8AMIIBigKCAYEA0Damo9DNCiQcgYmcUsY/
Gm3Y3fQIQseUuuwphnnleTl9/9m7F4KEIsj+f4vCFbcPc1iiKh8d9UE+brF2ShjR
QbggsPPjUtfMF8Z9HiokIkNf2uM4uduBuzoejgqyLpkr6fLFyKxh0a+nwB+O6hLI
8zJnt0BJZhA0pYds0eZJLdwArjJ4fi3H7q+64tSlHSXBQ+FETcBpk4bNTrSHIKYy
0FX7k5kW9QLlZ3Kr33x06TAROC92pyKSJrXYmN/85wDlZs0mTMmsc6M3tbAzDEWn
0ef0BCCowNgJunrzKzbEOHEcxmA8Y6bstPeasBL4W6/Vm3udRHueIE5N5ZQEbOhw
HcQcXTapPM3yw9sWO28pKGZKPTs+uqEsuFk3PfTWbpEmm/OLnx7PYsoHFlVvp0d/
h9a1LhC00b6/p9132U4JiRRHdGsWagVD7dwHahuWUT1Kx+uzkT01lJwzucurKtzX
1L31+I0V/bhUAvsFpuPA99jWRS3zoynA3/8ESHslLtU9AgMBAAE=
-----END PUBLIC KEY-----
EOF
))