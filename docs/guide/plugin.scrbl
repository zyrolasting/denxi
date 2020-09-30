#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "plugin"]{Writing a Plugin}

You can extend @binary using a Racket module as a @deftech{plugin}.

@section{Security Warning}

@tech{Plugins} run directly in @|binary|'s runtime, with the same
level of privilege as the OS-level user. This means that a plugin may
freely reconfigure @|binary|, if not outright harm your system.


@section{How to Define a Plugin}

Write a Racket module as you would normally, then save it to disk.
I'll start with a blank module at @litchar{~/xiden-plugin.rkt}, but
you can use a different name.

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


@section{Source Fetching}

Under the hood, @project-name understands finding packages from the
file system or over HTTP.  A user might express a package definition
source at the command line that @binary does not understand, such as
an SFTP URL or an URN meant for use with a specific service. A
@tech{plugin} can process that source on behalf of @|binary|.

You will want a plugin to process sources if you want @binary to obey
your conventions when referring to external resources. For example,
here's a command that will not work with @binary out of the box.

@verbatim|{$ xiden do +s vendor default us-east-1/storefront@0.2.0}|

Assume that in context, this command downloads a package definition
from a regional registry owned by your company. Let's say the package
definitions follow Semantic Versioning for consistency reasons.  The
work involved behind this request is non-trivial, so this section will
focus on the general problem.

@racketmod[#:file "~/xiden-plugin.rkt"
racket/base

(provide handle-source)

(define (handle-source user-string request-transfer)
  (and (supported? user-string)
       (begin (code:comment "Fetch and build"))))
]

@racket[user-string] is the string that a user provided.  Given the
command @litchar|{xiden +s vendor default foo.bar.baz}|, @racket[user-string]
is bound to @racket{foo.bar.baz}.

@racket[supported?] is a hypothetical procedure that guards the plugin
from doing work unless it recognizes the string as something it can
process. If @racket[handle-source] returns @racket[#f] as a
consequence of @racket[supported?] returning @racket[#f], then @binary
will raise an error alerting the user that it cannot make an
@tech{input} from @racket[user-string].

@racket[request-transfer] should be called in tail position.  It
accepts two arguments. The first is an input port that produces
bytes. The second is the estimated maximum number of bytes that port
is expected to produce. Assuming use of a file,
@racket[(request-transfer (open-input-file path) (+ (* 20 1024)
(file-size path)))] would be a valid call (The padding is for Mac OS
resource forks, which @racket[file-size] does not measure).

If you cannot estimate the number of bytes, you can bind
@racket[est-size] to @racket[+inf.0] to indicate no upper limit. By
default, @binary will decide to continue only if the user consents
to unlimited transfers.
