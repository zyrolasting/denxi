#lang scribble/manual

@require["../shared.rkt"
         @for-label[@except-in[denxi/pkgdef #%module-begin]
                    denxi/integrity
                    denxi/signature
                    racket/base]]

@title[#:tag "new-pkg"]{Package Definitions}

A @deftech{package definition} is a program, and this one creates a
familiar greeting in a text file.

@racketmod[#:file "definition.rkt"
denxi

(input "hello.txt"
  (artifact (text-source "Hello, world!") #f #f))

(output "default"
  (keep-input "hello.txt"))
]

We define an @racket[input] as a named source of data. You'll later
learn what the other terms mean, and how they can be adjusted to use
the network, verify downloads, and even use your own notation for
external resources.

We define an @racket[output] as a named subprogram that turns inputs
into files. We'll install that output in the next section.
