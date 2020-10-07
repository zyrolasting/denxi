#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Model}

@project-name builds dependencies atomically and deterministically
using @tech{package definitions}.  A package definition contains
discovery information and a declaration of program structure. The
program structure consists of @tech{package inputs}, package outputs,
and a package build procedure for processing. In this sense, a package
is a branded program that reliably reproduces exact files.

All of @|project-name|'s files are stored in a @tech{workspace}
directory. Barring direct user modifications, the content of a workspace
directory is always internally consistent due to transactional file I/O.

@project-name issues symbolic links as references to built files and
directories. This is how a dependent gains access to a dependency.
@|project-name| implements a garbage collector that deletes any file or
directory with no known symbolic links.


@section{User Experience Notice}

Instead of an @tt{install} or @tt{uninstall} command, @project-name expects a
user to define transactions with the filesystem and request links (read:
references) to the exact files they need. Some users might not appreciate an
unconventional interface, so some push-back is expected.

Maintainers should refrain from adding aliases that conform to traditional
package manager commands to @|project-name|, because they would not contribute
functionality. They should instead suggest defining such aliases as shell
scripts, which is a better medium for user preferences.

A more actionable complaint would address @|project-name|'s current requirement
for Racket values by every flag, including boolean short flags
(e.g. @litchar{-U '#t'}). This is a combination of
@racket[parse-command-line]'s interpretation of procedure arity and
@|project-name|'s insistence on full ability to override the runtime
configuration through several means.


@section{Security Notice}

Like any system that runs code from the Internet, @project-name can be used for
nefarious purposes. This attack surface is made complicated due to differences
in how Windows and Unix-like systems model permissions, and the flexibility of
@|project-name|'s runtime configuration.  @project-name offers security
features to mitigate some risks, but administrators should configure their
operating system to restrict the behavior of any user or group running a
@project-name process. Failing to do so allows room for arbitrary code
execution.
