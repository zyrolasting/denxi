#lang scribble/manual

@require["../../shared.rkt"]

@title{Basic Information}

To build all project deliverables, run @litchar{make} or @litchar{make
build}. To run the tests, run @litchar{make test}. To do both, run
@litchar{make build test}.

Xiden is versioned according to @secref["versioning" #:doc '(lib
"xiden/docs/guide/xiden-guide.scrbl")] in published package definitions. Do not
follow the versioning scheme defined in @secref["Package_Concepts" #:doc '(lib
"pkg/scribblings/pkg.scrbl")], even in the @litchar{info.rkt} file.


@section{User Experience Notice}

Instead of an @tt{install} or @tt{uninstall} command, Xiden expects a
user to define transactions with the filesystem and request links (read:
references) to the exact files they need. Some users might not appreciate an
unconventional interface, so some push-back is expected.

Maintainers should refrain from adding aliases that conform to traditional
package manager commands to Xiden, because they would not contribute
functionality. They should instead suggest defining such aliases as shell
scripts, which is a better medium for user preferences.

A more actionable complaint would address Xiden's current requirement
for Racket values by every flag, including boolean short flags
(e.g. @litchar{-U '#t'}). This is a combination of
@racket[parse-command-line]'s interpretation of procedure arity and
Xiden's insistence on full ability to override the runtime
configuration through several means.


@section{Security Notice}

Like any system that runs code from the Internet, Xiden can be used for
nefarious purposes. This attack surface is made complicated due to differences
in how Windows and Unix-like systems model permissions, and the flexibility of
Xiden's runtime configuration.  Xiden offers security
features to mitigate some risks, but administrators should configure their
operating system to restrict the behavior of any user or group running a
Xiden process. Failing to do so allows room for arbitrary code
execution.
