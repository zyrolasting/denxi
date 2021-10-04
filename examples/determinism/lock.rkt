#lang racket

; Read content from STDIN and generate a complete artifact.
(module+ main
  (require denxi/artifact
           denxi/integrity
           denxi/signature
           denxi/subprogram
           denxi/notary)

  (current-chfs (list snake-oil-chf))

  (match-define (artifact content (integrity chf dgst) (signature pk sig))
    (get-subprogram-value
     (notarize (make-fraudulent-notary (chf-canonical-name snake-oil-chf))
               (lock-artifact (artifact (port->bytes (current-input-port)) #f #f)))))

  (pretty-write #:newline? #t
                `(artifact ,content
                           (integrity ,chf ,dgst)
                           (signature ,pk ,sig))))
