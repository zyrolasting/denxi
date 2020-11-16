#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/path
                    xiden/logged
                    xiden/message
                    xiden/integrity
                    xiden/input-info
                    xiden/signature
                    xiden/string]
         "../../shared.rkt"]

@title{Resolving Inputs}

@defmodule[xiden/input-info]

A @deftech{package input} is an instance of @racket[input-info].

@defstruct*[input-info ([name string?] [sources (non-empty-listof string?)] [integrity (or/c #f integrity-info?)] [signature (or/c #f signature-info?)]) #:prefab]{
A structure representing a request for exact bytes.
}


@defproc[(input [name string?]
                [sources (non-empty-listof string?)]
                [integrity (or/c #f well-formed-integrity-info/c) #f]
                [signature (or/c #f well-formed-signature-info/c) #f])
         well-formed-input-info/c]{
An abbreviated @racket[input-info] constructor that performs stronger validation on its arguments.
}


@defthing[well-formed-input-info/c
          flat-contract?
          #:value (struct/c input-info
                            file-name-string?
                            (non-empty-listof string?)
                            (or/c #f well-formed-integrity-info/c)
                            (or/c #f well-formed-signature-info/c))]{
A contract that recognizes @tech{package inputs} suitable for use in
@tech{package definitions}.
}

@defproc[(input-ref [inputs (listof input-info?)] [name string?]) (or/c #f input-info?)]{
Returns the first element of @racket[inputs] @racket[I] where
@racket[(input-info-name I)] is @racket[name], or @racket[#f] if no
such element exists.
}


@defproc[(call-with-input [input input-info?] [proc (-> path-string? any)]) any]{
Returns @racket[(proc P)], where @racket[P] is a path to a symbolic link. That link
points to the file produced using @racket[(resolve-input input)].

@racket[call-with-input] deletes @racket[P] after @racket[proc]
finishes, and before returning control to the caller.

Use for creating references to dependencies that only apply during a
build. This allows an input's target file to remain eligible for
garbage collection.
}


@defproc[(keep-input! [input input-info?]) path-string?]{
Returns a path to a symbolic link derived from @racket[input].
The link is created as a side effect, and will not be deleted
by @racket[keep-input!].

Use for creating references to dependencies that persist after
a build. Any file referenced in this way will not be eligible
for garbage collection unless all links are removed.
}


@defproc[(resolve-input [input input-info?]) logged?]{
Returns a deferred computation that, when applied, acquires and
verifies the bytes for @racket[input].

The process will fail if the bytes do not meet the requirements
of @racket[input], if no bytes are available, or if the runtime
configuration does not place trust in the bytes.
}

@defproc[(make-input-expression-from-files
         [path path-string?]
         [#:local-name local-name string? (path->string (file-name-from-path path))]
         [#:byte-encoding byte-encoding (or/c #f xiden-encoding/c) 'base64]
         [make-sources (-> bytes? path-string? (non-empty-listof string?))]
         [message-digest-algorithm md-algorithm/c]
         [public-key-source string?]
         [private-key-path path-string?]
         [private-key-password-path (or/c #f path-string?) #f])
         list?]{
Returns an input expression (as a list datum) from several pieces of
information stored in files. All files must exist. @bold{Do not use
this procedure with untrusted data}.

For example, consider the following application:

@racketblock[
(make-input-expression-from-files
  "source-code.tar.gz"
  #:local-name "code.tar.gz"
  #:byte-encoding 'base64
  (lambda (digest path) (list "https://example.com/code.tar.gz"))
  'sha384
  "https://example.com/public-key.pem"
  "~/path/to/private-key.pem"
  "~/path/to/private-key-password.txt")]

@margin-note{@project-name uses OpenSSL subprocesses to sign digests.
To prevent *nix monitoring tools like @tt{top} from seeing your
private key's password, it sends the password to OpenSSL using a file
path.  This is why you cannot pass your password directly, but be sure
to securely delete the password file the moment you are done using
it.}

This takes an archive called @racket{source-code.tar.gz} from the file
system. In the package definition, it will be referred to simply as
@racket{code.tar.gz}. The source list is computed dynamically from
@racket[make-sources], which accepts the message digest (as bytes) and
a reference @racket[eq?] to @racket[path] as arguments.

The input expression will contain integrity information using a
@racket['sha384] digest. Finally, the signature information will
contain the exact public key source and a signature computed from the
private key and, optionally, the password used to access the private
key.

The output expression will look something like this:

@racketblock[
'(input "code.tar.gz"
        (sources "https://example.com/code.tar.gz")
        (integrity 'sha384 (base64 "..."))
        (signature "https://example.com/public-key.pem"
                   (base64 "...")))]

All expressions of byte strings are encoded using
@racket[byte-encoding].  If @racket[byte-encoding] is @racket[#f],
then byte strings are embedded directly in the output expression.

This procedure can be used with a REPL to help copy and paste input
expressions into a @tech{package definition}. A more convenient way to
realize this is to hold every argument except the input path constant.
This allows authors to define several inputs the same way.

@racketblock[
(define (mkinput . paths)
  (for/list ([p (in-list paths)])
    (make-input-expression-from-files
      p
      (lambda (digest also-p) (list (format "https://example.com/~a" (file-name-from-path also-p))))
      'sha384
      "https://example.com/public-key.pem"
      "~/path/to/private-key.pem"
      "~/path/to/private-key-password.txt")))]

}
