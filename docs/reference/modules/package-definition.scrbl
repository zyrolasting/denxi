#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/path
                    racket/pretty
                    syntax/modread
                    xiden/package-definition]
        xiden/package-definition
         "../../shared.rkt"]

@title{Package Definition API}

@defmodule[xiden/package-definition]

A @deftech{package definition} is a
@racket[PACKAGE_DEFINITION_MODULE_LANG] module as a syntax object
or a list. When a package definition is a list, it matches
@racket[package-definition-datum?]. Each package definition is used as
a Racket module that combines discovery information with build
instructions for @tech{packages}.


@section{Package Definition Basics}

@defproc[(package-definition-datum? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a list (not a syntax object)
suitable for use with @racket[get-package-definition-body]. This does
not validate the body of an implied Racket module.
}

@defthing[package-definition-variant/c flat-contract?
  #:value (or/c path? syntax? list? string? bytes? input-port?)]{
A contract that captures acceptable types when attempting to coerce a
Racket value to a @tech{package-definition} using @racket[read-package-definition].
}


@deftogether[(
@defthing[PACKAGE_DEFINITION_MODULE_LANG symbol?]
@defthing[PACKAGE_DEFINITION_READER_LANG symbol?]
)]{
Collection paths for a module language and reader extension used to
write package definitions.
}


@section{Package Definition I/O}

@defproc[(read-package-definition [variant package-definition-variant/c]) syntax?]{
Reads a Racket module form as a syntax object from
@racket[variant]. The method depends on the type of the value bound to
@racket[variant]. This procedure may raise
@racket[exn:fail:xiden:read-package-definition], an exception from the
@racket[read-syntax] provided from
@racket[PACKAGE_DEFINITION_READER_LANG], or an exception from
@racket[check-module-form].

If @racket[variant] is an input port, @racket[read-package-definition]
will attempt to read a Racket module. A reader guard will prohibit use
of any extension other than @racket[PACKAGE_DEFINITION_READER_LANG].
The module must also use @racket[PACKAGE_DEFINITION_MODULE_LANG].

If @racket[variant] is a path, then the behavior is equivalent to
applying @racket[read-package-definition] to an input port for the
file referenced by that path.

If @racket[variant] is a string or a byte string, then the behavior is
equivalent to applying @racket[read-package-definition] to an input port
opened using @racket[variant].

If @racket[variant] is a list, then the behavior is equivalent to
applying @racket[read-package-definition] to an input port opened
using @racket[(~s variant)].

If @racket[variant] is a syntax object, then the behavior is
equivalent to applying @racket[read-package-definition] to
@racket[(syntax->datum variant)] (where the contract will fail unless
the datum is a list). Since this procedure returns a syntax object on
a successful read, then using a syntax object as input has the effect
of verifying the object's readability as a @tech{package definition}.
The drawback is that the output syntax object will not have the same
location information or lexical informtaion.
}

@defstruct*[(exn:fail:xiden:read-package-definition exn:fail:xiden)
            ([variant package-definition-variant/c] [for-module? boolean?] [lang any/c]) #:prefab]{
Represents a failed evaluation of @racket[(read-package-definition
variant)] due to an incorrect reader extension or module language.

When @racket[for-module?] is @racket[#t], @racket[lang] is the datum
found in @racketid[module-path] position of an alleged @racket[module]
form.

When @racket[for-module?] is @racket[#f], @racket[lang] is
@racket[eq?] to the sole argument passed to
@racket[current-reader-guard] in the context of the module read.
}


@defproc[(write-package-definition! [#:pretty? pretty? boolean? #t]
                                    [#:exists exists (or/c 'error 'append 'update
                                                       'replace 'truncate 'truncate/replace)
                                                       'error]
                                    [datum package-definition-datum?]
                                    [variant (or/c path-string? output-port?)])
                                    void?]{
Writes a package definition to a location specified by @racket[variant].

If @racket[variant] is an output port, then bytes are written directly
to @racket[variant]. If @racket[pretty?] is @racket[#t], then the
output is formatted using a @litchar|{#lang}| reader extension
followed by entries from @racket[(get-package-definition-body datum)].
If @racket[pretty?] is @racket[#f], then the output is
@racket[(pretty-write #:newline? #t datum)].

If @racket[variant] is a path or path string, then the behavior is
equivalent to

@racketblock[
(call-with-output-file #:exists exists variant
  (λ (o) (write-package-definition! #:pretty? pretty? datum o)))]
}


@section{Package Definition Construction and Destructuring}

@defproc[(make-package-definition-datum [#:id id symbol? 'pkgdef] [body list?]) package-definition-datum?]{
Equivalent to @racket[`(module ,id ,PACKAGE_DEFINITION_MODULE_LANG ,@body)]
}

@defproc[(get-package-definition-body [datum package-definition-datum?]) list?]{
Returns the top-level forms of the module code in @racket[datum].
}


@section{Package Code Generation}

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
