#lang denxi

(name "example00-output")

(output "default"
        module-input := (input-ref "hello.rkt")
        (resolve-input module-input))

(input "hello.rkt"
       (artifact
        (lines-source #f
                      '("(module hello racket/base"
                        "  (provide say-hello)"
                        "  (define (say-hello)"
                        "    (displayln \"Hello!\")))"))
        #f
        #f))
