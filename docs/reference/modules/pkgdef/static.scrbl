#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/path
                    racket/pretty
                    syntax/modread
                    xiden/pkgdef/static]
         "../../../shared.rkt"]

@title{Static Operations for Package Definitions}

@defmodule[xiden/pkgdef/static]

A @deftech{package definition} is a
@racket[PACKAGE_DEFINITION_MODULE_LANG] module as a syntax object or a
list. When a package definition is a list, it matches
@racket[package-definition-datum?]. Each package definition is used as
a Racket module that combines discovery information with build
instructions for @tech{packages}.

@deftogether[(
@defthing[PACKAGE_DEFINITION_MODULE_LANG symbol?]
@defthing[PACKAGE_DEFINITION_READER_LANG symbol?]
)]{
Collection paths for a module language and reader extension used to
write package definitions.
}


@section{Package Definition Construction and Destructuring}

@defproc[(make-package-definition-datum [#:id id symbol? 'pkgdef] [body list?]) package-definition-datum?]{
Equivalent to @racket[(make-racket-module-datum #:id id PACKAGE_DEFINITION_MODULE_LANG body)]
}

@defproc[(get-package-definition-body [datum package-definition-datum?]) list?]{
Returns the top-level forms of the module code in @racket[datum].
}

@defthing[bare-pkgdef? flat-contract?
          #:value (struct/c bare-racket-module symbol?
                            PACKAGE_DEFINITION_MODULE_LANG
                            list?)]{
A contract that matches a @tech{bare} @tech{package definition}.
}


@defthing[package-definition-datum? predicate/c]{
Equivalent to @racket[(racket-module-code? PACKAGE_DEFINITION_MODULE_LANG v)].
}

@section{Package Definition Analysis}

The following procedures return data found from matching against the
code of a @tech{bare} @tech{package definition}. They do not run
package definition code.

@defproc[(get-static-abbreviated-query [pkgdef bare-pkgdef?]) xiden-query-string?]{
Returns a @tech{package query} containing the provider, package,
edition, and revision number in @racket[pkgdef].
}

@defproc[(get-static-simple-string [pkgdef bare-pkgdef?] [id symbol?]) any/c]{
Equivalent to @racket[(get-static-simple-value stripped id "default")].

The return value is assumed to be a string, but might not be.
}

@defproc[(get-static-inputs [pkgdef bare-pkgdef?]) list?]{
Returns a list of all input expressions in @racket[pkgdef].
}

@defproc[(get-static-simple-value [pkgdef bare-pkgdef?] [id symbol?] [default any/c]) any/c]{
Searches the top-level code of @racket[pkgdef] for a S-expression of
form @racket[(id val)]. Returns the datum in @racket[val]'s position,
or @racket[default] if no such expression exists.
}


@section{Package Definition Code Generation}

@defproc[(make-input-expression-from-files
         [path path-string?]
         [#:local-name local-name string? (path->string (file-name-from-path path))]
         [#:byte-encoding byte-encoding (or/c #f xiden-encoding/c) 'base64]
         [#:md-algorithm message-digest-algorithm md-algorithm/c 'sha384]
         [make-sources (-> bytes? path-string? (non-empty-listof any/c))]
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

@margin-note{Xiden uses OpenSSL subprocesses to sign digests.
To prevent *nix monitoring tools like @tt{top} from seeing your
private key's password, it sends the password to OpenSSL using a file
path.  This is why you cannot pass your password directly, but be sure
to securely delete the password file the moment you are done using
it.}

This takes an archive called @racket{source-code.tar.gz} from the file
system. In the package definition, it will be referred to simply as
@racket{code.tar.gz}. The source list is computed dynamically from
@racket[make-sources], which accepts the message digest (as bytes) and
a reference @racket[eq?] to @racket[path] as arguments. Note that the
sources are used literally in the output expression, so procedures
like @racket[(lambda (digest path) `((from-file ,(bytes->string/utf-8
digest))))] are valid.

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

@section{Package Definition Transformations}

@defproc[(override-inputs [pkgdef bare-pkgdef?] [input-exprs list?]) bare-pkgdef?]{
Functionally replaces any @tech{package inputs} in @racket[pkgdef]
that share a name with at least one element in @racket[input-exprs].

If multiple expressions in @racket[input-exprs] share a name, only the
last occurrance will be used in the output.
}
