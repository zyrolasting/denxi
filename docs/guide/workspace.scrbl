#lang scribble/manual

@require["../shared.rkt" @for-label[racket/base]]

@title[#:tag "workspace"]{Workspace Directories}

All of @binary's disk activity takes place in an initally empty
@deftech{workspace} directory called @|wsdir|. @binary organizes files in
@wsdir according to the
@hyperlink["https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html"]{Filesystem
Heirarchy Standard v3.0}. This makes a @tech{workspace} a valid target for
@tt{chroot}. You can therefore work with @binary to construct a bootable or
jailed application.

Everything related to @binary is in a @tech{workspace}, including its
configuration, logs, installed files, cached files, and database. Any one of
your projects can have its own @wsdir, and therefore its own configuration and
dependencies. Each @wsdir is isolated unless you link them together
yourself. You can define the actual root directory of a Linux system as a
workspace for system-wide impact.

When @project-name starts, it will select a @deftech{target workspace} by
searching for a @|wsdir| directory.  It first checks if @wsdir is in the
@racket[(current-directory)].  Failing that, @project-name will check each
parent directory for @|wsdir|. If @wsdir does not exist, then @binary will
create a new one in @racket[(current-directory)].
