#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden Reference}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is the API reference for Xiden.

For all documentation, see @other-doc[xiden-index].

@bold{Warning}: The API is currently unstable.

Xiden uses @tech{package definitions} to build directories and issue
symbolic links to those directories.  A garbage collector deletes any
directory it created once it has no known symbolic links. When given
the oppurtunity to perform file or network I/O, Xiden will proceed
only with the user's consent.

Since dependency management is subjective, Xiden allows users to
define custom @tech{launchers} that configure Xiden with privileged
code. For security, Xiden also ships with a launcher called
@litchar{xiden} that defaults to a zero-trust configuration.

This section covers the modules that cooperate to these ends.

@table-of-contents[]

@include-section{modules/cli.scrbl}
@include-section{modules/cmdline.scrbl}
@include-section{modules/cli-flag.scrbl}
@include-section{modules/package.scrbl}
@include-section{modules/pkgdef.scrbl}
@include-section{modules/query.scrbl}
@include-section{modules/launcher.scrbl}
@include-section{modules/subprogram.scrbl}
@include-section{modules/artifact.scrbl}
@include-section{modules/message.scrbl}
@include-section{modules/input.scrbl}
@include-section{modules/integrity.scrbl}
@include-section{modules/signature.scrbl}
@include-section{modules/dig.scrbl}
@include-section{modules/main.scrbl}
@include-section{modules/source.scrbl}
@include-section{modules/version.scrbl}
@include-section{modules/state.scrbl}
@include-section{modules/setting.scrbl}
@include-section{modules/format.scrbl}
@include-section{modules/url.scrbl}
@include-section{modules/string.scrbl}
@include-section{modules/l10n.scrbl}
@include-section{modules/printer.scrbl}
@include-section{modules/archive.scrbl}
@include-section{modules/system.scrbl}
@include-section{modules/monad.scrbl}
@include-section{modules/file.scrbl}
@include-section{modules/openssl.scrbl}
@include-section{modules/port.scrbl}
@include-section{modules/security.scrbl}
@include-section{modules/racket-module.scrbl}
@include-section{modules/codec.scrbl}
@include-section{modules/notary.scrbl}
@include-section{modules/lock.scrbl}
