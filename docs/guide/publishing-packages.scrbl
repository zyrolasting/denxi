#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Publishing Packages}

To prepare a package for use on a server, use the @litchar{bundle}
command.

@verbatim|{
$ zcpkg bundle my-pkg '\.rktd?$' '\.scrbl$'
}|

When you bundle a package, you specify the package's directory and
Perl-flavor regular expressions. The file paths matching the patterns
are included in the bundle.

The @litchar{bundle} command outputs an archive and an extended @tech{package definition}.

Use @litchar{-r} to use built-in patterns that match most Racket source modules.

@verbatim|{
$ zcpkg bundle -r my-pkg
}|

Use @litchar{-s} to write the output files where @litchar{zcpkg serve} can find
them.  This is to say that if you don't use @litchar{-s}, then you probably
intend to upload your bundle as a package author.

@verbatim|{
$ zcpkg bundle -rs my-pkg
}|

Finally, you can specify a private key to sign the bundle's digest.

@verbatim|{
$ zcpkg bundle --private-key-path key.pem -rs my-pkg
}|
