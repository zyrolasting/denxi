#lang scribble/manual

@require[@for-label[racket
                    @except-in[xiden/pkgdef #%module-begin]]]

@title{Add Custom Metadata to Package Definitions}

Package definitions include forms for common metadata such as
@racket[url], @racket[tags], and @racket[description]. If you want to
store other information in your definition, then you can use the
@racket[metadatum] form.

@racketblock[
(metadatum support-email "support@example.com")
]

@racket[metadatum] works like @racket[define], in that you can
associate an identifier with a value. When someone uses
@racket[require] or one of its variants on a package definition, they
can inspect an expanded @racket[metadata] binding to see all
user-defined metadata.

@racketinput[(module anon xiden/pkgdef (metadatum support-email "support@example.com"))]
@racketinput[(require 'anon)]
@racketinput[metadata]
@racketresult['#hasheq((support-email . "support@example.com"))]

Metadata can only be literal strings, and are not meant for use in
program logic.
