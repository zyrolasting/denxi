#lang scribble/manual

@require[@for-label[racket/base]
         "../shared.rkt"]

@title[#:style '(toc)]{Xiden: API Reference}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This is the API reference for Xiden.

For a high-level overview, see @other-doc['(lib "xiden/docs/guide/xiden-guide.scrbl")].

@bold{Warning}: The API is currently unstable.

@table-of-contents[]
@include-section{model.scrbl}
@include-section{modules/pkgdef.scrbl}
@include-section{modules/package.scrbl}
@include-section{modules/main.scrbl}
@include-section{modules/version.scrbl}
@include-section{modules/query.scrbl}
@include-section{modules/workspace.scrbl}
@include-section{settings.scrbl}
@include-section{modules/message.scrbl}
@include-section{modules/logged.scrbl}
@include-section{modules/codec.scrbl}
@include-section{cli.scrbl}
@include-section{verification.scrbl}
@include-section{modules/racket-module.scrbl}
@include-section{modules/source.scrbl}
@include-section{modules/catalog.scrbl}
@include-section{modules/input-info.scrbl}
@include-section{modules/plugin.scrbl}
@include-section{modules/format.scrbl}
@include-section{modules/exn.scrbl}
@include-section{modules/url.scrbl}
@include-section{modules/string.scrbl}
@include-section{modules/l10n.scrbl}
@include-section{modules/printer.scrbl}
@include-section{modules/archive.scrbl}
@include-section{modules/system.scrbl}
@include-section{modules/monad.scrbl}
@include-section{modules/file.scrbl}
@include-section{modules/openssl.scrbl}
@include-section{modules/security.scrbl}
@include-section{maintenance/maintenance.scrbl}
