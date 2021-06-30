#lang scribble/manual

@require["../shared.rkt"
         @for-label[@except-in[xiden/pkgdef #%module-begin]
                    xiden/integrity
                    xiden/signature
                    racket/base]]

@title[#:tag "new-pkg"]{Packages}

A @deftech{package definition} is a program. The difference between a
package definition and a package is like the difference between a
program and a process.

This package definition declares a @tech/xiden-reference{version} and
a text file.

@racketmod[#:file "definition.rkt"
xiden

(name "my-first-package")
(edition "default")
(revision-number 0)
(revision-names "initial" "oldest")

(output "default"
  (keep-input "hello.txt"))

(input "hello.txt"
       (artifact (text-source "Hello, world!") #f #f))
]

We define a package @racket[input] as a named source of data.  In this
case, we will define the data in the definition itself using an
@racket[artifact] containing hard-coded text. You can adjust an
artifact to fetch data over HTTP, verify the download, and even check
for a valid signature. You will learn these features in the examples
at the end of the guide.

We define a package @racket[output] as a named subprogram that builds
files using package inputs. When users install software, they select a
package output to install. The output is then installed within a
file-system transaction.

I'm skipping a mountain of information because there are many other
terms and nuances to this language. For now, it is enough know that
package definitions define may define multiple inputs and outputs
under a precise identity.
