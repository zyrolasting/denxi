#lang scribble/manual

@require["../shared.rkt"
         @for-label[@except-in[xiden/pkgdef #%module-begin]
                    xiden/integrity
                    xiden/signature
                    racket/base]]

@title[#:tag "new-pkg"]{Packages}

Here we discuss a program that will install software in the next
section. The program is a @deftech{package definition}, because it
defines possible software distributions. The difference between a
package definition and a package is like the difference between a
program and a process.

This package definition declares a @tech/xiden-reference{version} and
an archive for extraction.

@racketmod[#:file "definition.rkt"
xiden

(name "my-first-package")
(edition "default")
(revision-number 0)
(revision-names "initial" "oldest")

(output "default"
  (extract-input "archive.tgz"))

(input "archive.tgz"
  (artifact
    (http-source "https://sagegerard.com/xiden-guide/archive.tgz")
    (integrity
      'sha1
      (http-source "https://sagegerard.com/xiden-guide/archive.sha1"))
    (signature
      (http-source "https://sagegerard.com/xiden-guide/public.pem")
      (http-source "https://sagegerard.com/xiden-guide/archive.sha1.sign"))))
]

A package input is a named source of data.  We define package inputs
using the @racket[input] term. In this case we define an archive file
on a server using an @racket[artifact]. An artifact holds data along
with the means to verify that the data is correct [@topic{integrity}]
and comes from someone we trust [@topic{signature}].

Careful readers will notice that content and verification information
come from the same place. Don't be alarmed. Xiden does not volunteer
trust in this situation [@topic{determinism}].

A package output is a named subprogram that builds files using package
inputs. We define package outputs using the @racket[output] term. When
users install software, they select a package output to install. The
output is then run in a transaction against a new directory on
disk. As an aside, package outputs use monadic types [@topic{monads}].

There are many other terms and nuances to this language, but this is
enough for us to proceed.
