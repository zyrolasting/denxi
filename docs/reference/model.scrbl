#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title{Model}

@project-name issues symbolic links as references to built files and
directories. @project-name builds a user's dependencies atomically and
deterministically using @tech{package definitions}.  A package
definition conveys discovery information and rough program structure
(i.e., inputs, outputs, and processing). In this sense, a package is a
branded program that reliably reproduces an exact directory tree.

All of @|project-name|'s files are stored in a @tech{workspace}
directory. Barring direct user modifications, the content of a
workspace directory is always internally consistent due to
transactional file I/O.

@|project-name| implements a garbage collector that deletes files
with no symbolic links issued by @|project-name|.

A user accustomed to a traditional package manager would expect to run
@tt{install} and @tt{uninstall} commands. @project-name instead
expects a user to define transactions with the filesystem and request
links (read: references) to the exact files they need. The garbage
collector removes any file with no references.

Like any system that works with code from the Internet, @project-name
can be used for nefarious purposes. This is largely due to differences
in how Windows and Unix-like systems model permissions, and the
flexibility of the runtime configuration.  @project-name offers
security features to mitigate some risks, but administrators should
configure their operating system to restrict the behavior of any user
or group running a @project-name process.
