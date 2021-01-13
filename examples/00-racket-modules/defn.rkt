#lang xiden

(output "default"
        module-input := (input-ref "hello.rkt")
        (resolve-input module-input))

(input "hello.rkt"
       (sources (lines-source #f
                              '("(module hello racket/base"
                                "  (provide say-hello)"
                                "  (define (say-hello)"
                                "    (displayln \"Hello!\")))"))))
