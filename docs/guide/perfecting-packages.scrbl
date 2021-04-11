#lang scribble/manual

@require["../shared.rkt" @for-label[@except-in[xiden/pkgdef #%module-begin] racket/base]]

@title[#:tag "simple-pkgdef"]{Perfecting Package Definitions}

This section supplements @secref{new-pkg}. You will find it useful if
you want to ease writing package definitions, or set up custom
workflows.


@section{Adapting Inputs to Change}

Package inputs that use the same correct integrity information and
signatures are guarenteed to produce the same bytes from a trusted
source.

This is great for reproducible builds, but it can slow you down. In
the below example, changing the source means changing the integrity
information. Changing the integrity information means changing the
signature.

@racketblock[
(input "anon"
       (byte-source #"84n\24...")
       (integrity 'sha384 #"47u2c...")
       (signature #"fa9v\0..."
                  #"h!,094..."))
]

Altogether, writing an updating these expressions is a chore. One way
to ease this effort is to leverage conventions from a remote source.

@racketblock[
(input "anon"
       (http-source "https://example.com/my-file")
       (integrity 'sha384 (http-source "https://example.com/my-file.sha384"))
       (signature
         (http-source "https://example.com/public.pem")
         (http-source "https://example.com/my-file.sha384.sig")))
]

This approach decouples the package definition from the content. When
an administrator updates the file, digest, or signature, the package
definition does not have to change. Xiden will still protect the user
from unwanted data, even if the server is malicious.

This is still a lot to type, and Xiden cannot assume that you want to
commit to SHA-384, or the host located at @racket{example.com}. Other
sections in this document cover ways to avoid writing @racket[input]
expressions altogether.


@subsection{Declaring Catalog Dependencies}

One way to abbreviate inputs is to express them as a conventional
dependency list.

@racketblock[
(dependencies "example.com:example-package"
              "racket-lang.org:installer:minimal")
]

@racket[dependencies] adds inputs in terms of the @racketid[catalog]
provided by a @tech{plugin}. Those inputs must produce package
definitions.


@section{Comissioning Packages}

@racket[comission] instructs Xiden to delegate package construction
work to a @tech/xiden-reference{plugin}. You can then use the plugin
to define the conventions for an input via an
@racket[override-package] procedure.

@racketblock[
(comission "my-file" "my-file2" ...)
]

A plugin can implement @racket[commission] as follows:

@racketmod[racket/base

(provide (all-defined-out))

(define chf 'sha384)

(define (mksrc . v)
  (http-source (apply ~a "https://example.com/" v)))

(define (override-package p name)
  (struct-copy package p
    [inputs (cons (input name
                         (mksrc name)
                         (integrity chf (mksrc name "." chf))
                         (signature (mksrc "public.pem")
                                    (mksrc name "." chf ".sig")))
                  (package-inputs p))]))
]

This plugin adds an input to the package definition using the data and
conventions of a particular host. It can be further extended to assume
that all inputs are archives, and the added input comes with an output
that extracts the correct archive.

With all of that decided, the package definition only requires
information the package author finds relevant.

@racket[comission] is a thin wrapper over a call to
@racket[override-package] that only adds the current package as a
formal argument. You can adjust the signature of
@racket[override-package] freely to handle more complex conventions.

@racketblock[
(code:comment "Change override-package like you would any other procedure")
(comission #:chf 'sha3-384
           "my-file"
           "other-file")
]

It is possible to reduce an entire package definition to a single use
of @racket[comission], but the drawback is that the package
definitions must be paired with the correct
@tech/xiden-reference{plugin} to work.


@section{Abstract and Concrete Package Inputs}

An @deftech{abstract package input} (or just “abstract input”) is a
@tech{package input} with only a name.

@racketblock[(input "server.rkt")]

By contrast, @deftech{concrete package inputs} are package inputs that
include a @tech/xiden-reference{source}. A concrete package input does
not have to include integrity information or a signature.

Abstract inputs cannot be used to fetch exact bytes. Use them to
require the end user to define their own implementation for
well-defined interfaces, or to provide custom input for builds.

Xiden prohibits overriding abstract inputs with other abstract inputs,
and any override is subject to the same runtime restrictions.
