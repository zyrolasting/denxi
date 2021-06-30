#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/format
                    racket/string
                    xiden/artifact
                    xiden/dig
                    xiden/dig/filesystem
                    xiden/dig/http
                    xiden/message
                    xiden/integrity
                    xiden/crypto
                    xiden/query
                    xiden/signature
                    xiden/source
                    xiden/string
                    xiden/url]]

@title{HTTP Client Shovels}

@defmodule[xiden/dig/http]

@defproc[(make-http-shovel [base-url url-variant?]
                           [optional-chf (or/c #f symbol?) #f]
                           [optional-pubkey (or/c #f source-variant?) #f])
                           shovel/c]{
Returns a @tech{shovel} used to find artifacts @italic{anticipating}
an HTTP server's response (See @secref{http-shovel-caveats}).

The shovel accepts a @racket[url-variant?] value as an argument. If a
different value type is provided, the shovel behaves like
@racket[broken-shovel]. Otherwise, the shovel will create an artifact
with @racket[http-source] components w.r.t. @racket[base-url]. Path
and query string elements from the argument are appended to
@racket[base-url]'s respective fields. This implies that duplicate
query string keys are allowed.

@bold{Invariant:} All @racket[http-source]s in created artifacts must
yield the raw, unencoded bytes of the requested resource. This implies
that HTTP response headers must include @litchar|{Content-Type:
application/octet-stream}|.

If @racket[optional-chf] is @racket[#f], then an output artifact will
always lack both integrity information and signature information.
Otherwise, the artifact will contain integrity information using the
given CHF, along with a corresponding @racket[(~a "." optional-chf)]
extension to the last path element of the URL used to fetch the
digest.

If @racket[optional-pubkey] is @racket[#f], then an output artifact
will always lack signature information. Otherwise, the artifact will
contain signature information with @racket[optional-pubkey] as the
public key's source, along with a corresponding @racket[(~a "."
optional-chf ".sig")] extension to the last path element of the URL
used to fetch the signature.

Examples follow. @bold{Arguments to @racket[http-source] are shown as
URL strings for brevity. The actual output will include instances of
@racket[url].}

Given a shovel with no additional expectations...

@racketblock[
(define dig (make-http-shovel "https://example.com"))
]

...both @racket[(dig "/some-file")] and @racket[(dig (string->url
"/some-file"))] will produce

@racketblock[
(artifact (http-source "https://example.com/some-file") #f #f)
]

Given a shovel with all expectations...

@racketblock[
(define dig
        (make-http-shovel "https://example.com"
                          'sha3-384
                          (http-source "https://example.com/public.pem")))
]

...both @racket[(dig "/some-file")] and @racket[(dig (string->url
"/some-file"))] will produce

@racketblock[
(artifact (http-source "https://example.com/some-file")
          (integrity 'sha3-384
                     (http-source "https://example.com/some-file.sha3-384"))
          (signature (http-source "https://example.com/public.pem")
                     (http-source "https://example.com/some-file.sha3-384.sig")))
]

}

@section[#:tag "http-shovel-caveats"]{Caveats for HTTP Shovels}

Due to the volume of possible network conditions and API definitions
for servers, @racketmodname[xiden/dig/http] provides no mechanisms for
checking @tech{artifact} availability, and no @tech{package query
canon} for @tech{package queries}. Therefore,
@racket[make-http-shovel] will return artifacts that may produce no
content when they are actually used. @racket[make-http-shovel] only
offers reasonable, minimal defaults for downloading compatible
artifacts that follow the same naming conventions as
@racket[make-filesystem-shovel].

If you want a client to check resource availability in advance of
artifacts, you will need to implement your own HTTP shovel. If you
want an HTTP server to work with @racket[make-http-shovel], you can
simply serve the files from a directory suitable for
@racket[make-filesystem-shovel]. This will add an invariant where the
shovel and the server must agree in advance about what files are
available, which you can represent as arguments to
@racket[make-http-shovel].

Those implementing services will also need to consider the
relationship of @tech{package queries} to @tech{artifacts}
representing @tech{package definitions}, and implement a @tech{package
query canon} to allow that transition. All of the related machinery
for distributing artifacts over the network can be delivered to
clients in a @tech{launcher}, which warrants additional scrutiny since
launchers have all privileges of the process' user.
