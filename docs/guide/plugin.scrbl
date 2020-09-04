#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Writing a Plugin}

You can extend @binary using a Racket module as a @deftech{plugin}.


@section{Security Warning}

@tech{Plugins} run directly in @|binary|'s runtime, with the same
level of privilege. This means that a plugin may freely reconfigure
@|binary|, if not outright harm your system.

Plugins should be carefully vetted by a programmer and/or
administrator before use, because @binary will not vet it for you.
Using a plugin from the Internet made by an untrusted party is a
@bold{bad idea}.


@section{How to Define a Plugin}

Write a Racket module as you would normally, then save it to disk.
I'll start with a blank module at @litchar{~/xiden-plugin.rkt}.

@racketmod[#:file "~/xiden-plugin.rkt"
racket/base

(code:comment "...")
]


Next, specify a complete path to that module in @racket[XIDEN_PLUGIN_MODULE].
You can change that setting using your chosen method from @secref{config}.

The plugin is then installed. @binary will use it according to the
following sections.

@section{There is Only One Plugin}

According to @|binary|, the module in @racket[XIDEN_PLUGIN_MODULE] is
the @italic{only} source of extensions. If you have several plugins,
then you need to leverage Racket's module system to combine their
functionality.

@racketmod[#:file "~/xiden-plugin.rkt"
racket/base

(provide ...)

(require (rename-in "alpha-plugin.rkt" ...)
         (only-in "beta-plugin.rkt" ...))
]


@section{A Plugin Does Not Need @binary}

@binary passes everything a plugin needs to that plugin's procedures.
While you can @racket[require] @|binary|'s modules, this is not
necessary. This keeps plugins easy to test because you can simulate
how well a plugin works with @binary by testing with different
arguments.


@section{Plugin Operations}


@subsection{Source Fetching}

A user might express a package definition source at the command line
that @binary does not understand. A @tech{plugin} can process that
source on behalf of @|binary|.

You will want a plugin to process sources if you want @binary to obey
your conventions when referring to external resources. For example,
here's a command that will not work with @binary out of the box.

@verbatim|{$ xiden install us-east-1/storefront@0.2.0}|

Assume that in context, this command downloads a package definition
from a regional registry owned by your company. Let's say the package
definitions follow Semantic Versioning for consistency reasons.  The
work involved behind this request is non-trivial, so this section will
first focus on the general problem. We'll cover how a plugin handles
this specific problem in @secref{real-world-source-fetch}.


@subsubsection{Basic Program Structure}

Let's hand-wave over all implementation details so we can focus on the
overall contract.

@racketmod[#:file "~/xiden-plugin.rkt"
racket/base

(provide handle-source)

(define (handle-source make-input-file make-output-directory make-link user-string)
  (and (supported? user-string)
       (begin (code:comment "Fetch and build"))))
]

@racket[user-string] is the actual string that a user provided.  Given
the command @litchar|{xiden install foo.bar.baz chookie.bon.bon}|,
@binary will call @racket[handle-source] twice. The first call binds
@racket[user-source] to @racket{foo.bar.baz}, and the second call
binds @racket[user-source] to @racket{chookie.bon.bon}.

@racket[supported?] is a hypothetical procedure we'll write later.  It
guards the plugin from doing work unless it recognizes the
string as something it can process. If @racket[handle-source] returns
@racket[#f] as a consequence of @racket[supported?] returning
@racket[#f], then @binary will raise an error alerting the user that
it cannot make an @tech{input} from @racket[user-string].


@subsubsection{Creating Input Files}

@defproc[(make-input-file [in-port input-port?] [est-size (or/c #f exact-positive-integer?)]) complete-path?]{
Saves a new file in the @tech{workspace} to act as an @tech{input}.

A call looks like @racket[(make-input-file in-port est-size)], where
@racket[in-port] is an input port that produces bytes from somewhere,
and @racket[est-size] is the estimated number of bytes that
@racket[in-port] will produce. @binary will close @racket[in-port]
before returning control. @racket[make-input-file] returns a path to a
new file created in the @tech{workspace}.

@binary uses the arguments to @racket[make-input-file] to save bytes
from an arbitrary source to disk. If @racket[in-port] produces more
bytes than @racket[est-size], then @binary will immediately close
@racket[in-port], delete the file it was writing, and then blame the
source for returning more bytes than expected.

This call copies a file to the @tech{workspace}.

@racketblock[
(define input-path
  (let ([path "/path/to/some-file.txt"])
    (make-input-file (open-input-file path)
                     (+ (* 1024 20) (code:comment "Add a few KiBs of padding for Mac OS resource forks")
                        (file-size path)))))
]
}

@subsubsection{What if I Cannot Estimate the Number of Bytes?}

You can bind @racket[est-size] to @racket[#f] to indicate that you
cannot estimate the size. By default, @binary will take this to mean
that it should not read any bytes to avoid potentially taking up too
much space.

However, a user may set @racket[XIDEN_CONSENT_UNLIMITED_FETCH] to
consent to reading bytes from @racket[in-port] until @racket[eof].

@subsubsection{Creating Links to Input Files}

@defproc[(make-link [path complete-path?]) void?]{

Create a symbolic link pointing to @racket[path]. @racket[path] must
be a path returned from @racket[make-input-file] or
@racket[build-derivation].

We can modify the example from @racket[make-input-file] to use
@racket[make-link]. From there we can write a full implementation
of @racket[handle-source].

@racketblock[
(define (handle-source make-input-file make-link user-string)
  (and (file-exists? user-string)
       (make-link (make-input-file (open-input-file user-string)
                              (+ (* 1024 20) (code:comment "Padding for MacOS resource forks")
                              (file-size user-string))))))
]

This is a good approximation of what @binary does by default when
finding local files to use as @tech{inputs}.
}

@subsubsection{Creating Derivations}

@defproc[(build-derivation [racket-module-path input-path?]
                           [exprs list?]
                           [input-files (non-empty-listof input-path?)])
         complete-path?]{
Creates a @tech{derivation} in the @tech{workspace} in terms of @tech{inputs}.
Returns a path to the derivation's output directory.

The derivation is created by evaluating each of the given
@racket[exprs] in a sandboxed evaluator, in the context of
@racket[racket-module-path].  Notice that @racket[racket-module-path]
must be an @tech{input}, because packages are built solely in terms of
inputs, and the result of a package build is a @tech{derivation}.

}


@subsubsection{A Real-World Example}

@racketmod[#:file "~/xiden-plugin.rkt"
racket/base

(provide handle-source)

(define region-pattern-string
  (string-join
    '("(afr|aus|eur|us)"
      "(east|west|north|south)"
      "[1-3]")
    "-"))

(define package-definition-pattern-string
  "[a-z][a-z0-9-]+@(\\d+\\.){2}\\d+")

(define registry-px
  (pregexp
    (string-join
      (list region-pattern-string
            package-definition-pattern-string)
      "/")))

(define (handle-source link-name user-string)
  (if (regexp-match? registry-px user-string)
      (begin (code:comment "Fetch and build"))
      (rex exn:xiden:source:unsupported user-string)))
]
