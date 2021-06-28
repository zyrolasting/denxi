#lang scribble/manual

@require[@for-label[racket
                    @except-in[xiden/pkgdef #%module-begin]]]

@title[#:tag "os-support"]{Limit Packages to Specific Operating Systems}

Racket is cross-platform, but your package might not be.  Limiting OS
support allows you to make reasonable assumptions about available
binaries (e.g. GNU coreutils), and for offering tailored experiences
to users.

You can declare the operating systems you support by writing
@racketid[os-support] in a package definition with a list of
acceptable values of @racket[(system-type 'os)].

This example is a statement of support for UNIX-like systems, Windows,
and MacOSX.

@racketblock[
(os-support unix windows macosx)
]

By default, Xiden assumes each package definition will work on every
operating system. While this means you don't have to include the above
line in a package definition, you may wish to do so anyway for the
sake of being explicit.
