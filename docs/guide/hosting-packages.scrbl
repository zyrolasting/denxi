#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Hosting Packages}

You can serve packages using the @litchar{serve} command.

@verbatim|{
$ zcpkg serve
}|

The server keeps its own directory of packages bundled into archives.

@tt{GET /info/<query>} returns the contents of a @tech{package definition} in a
UTF-8 @racket[#"text/plain"] body.  The selected package definition corresponds
to the @italic{latest} revision of a package named in @tt{<query>}.
In addition to the fields defined in @secref{new-pkg}, the metadata
will include integrity information and a signature for the package.

@tt{GET /artifact/<query>} behaves like @tt{/info}, except the response
is a GZipped TAR archive file containing the package.

Currently, the service does not support uploading packages.  Adding
packages to the service is a manual process for an administrator.
