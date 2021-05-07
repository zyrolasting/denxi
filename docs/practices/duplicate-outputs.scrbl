#lang scribble/manual

@require[@for-label[racket
                    xiden/archive
                    @except-in[xiden/pkgdef #%module-begin]]
         "../shared.rkt"]

@title[#:tag "dupe-outputs"]{Avoid Duplicate Outputs of Different Names}

Xiden assumes that different @tech/xiden-guide{package outputs}
necessarily produce different content.  Let's say we define an output
to act as an alias of another.

@racketblock[
(output "default" (extract-input "default.tgz"))
(output "full" (extract-input "default.tgz"))
(output "minimal" (extract-input "minimal.tgz"))]

This works, but if a user requests the @racket{full} output and then
the @racket{default} output, then the same archive would be extracted
twice into different directories.  This pollutes the disk with
redundant data, which is probably not what you want. If you believe
that two outputs are equivalent, then combine them into one output.

