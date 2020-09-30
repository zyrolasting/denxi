#lang scribble/manual

@require["../shared.rkt"]

@title{Setup}

This section covers how to install @binary on your system.

@section{Required Programs}

First, make sure the following programs are available in your @tt{PATH}.

@itemlist[
@item{SQLite 3.24.0+. Verify with @litchar{sqlite3 -version}}
@item{Racket 7.0+. Verify with @litchar{racket -v}}
@item{OpenSSL 0.9.8+. Verify with @litchar{openssl version}}
]

@section{Step 1: Get the Code}

There are currently no pre-built binaries for @|project-name|, so we need to
build from source. The goal of this step is to get a directory named
@binary somewhere on your disk.

You can either use Git or download an archive.

@subsection{Method: Use Git}

If you wish to use Git, you can clone the default branch from @|project-name|'s
repository. If you use SSH, then run @litchar|{git clone
git@github.com:zyrolasting/xiden.git}|.  Or, if you use HTTPS, then run
@litchar|{git clone https://github.com/zyrolasting/xiden.git}|

Either command will leave a @binary directory in your working directory,
and you can move on to Step 2.

@subsection{Method: Download Archive}

If you do not have Git, then you can download a ZIP file
of the latest source code from the following link:

@centered[@hyperlink["https://github.com/zyrolasting/xiden/archive/master.zip"]{https://github.com/zyrolasting/xiden/archive/master.zip}]

You can then extract the archive using whatever tool you prefer. Just make
sure that the source files are inside of a directory named @|binary|.

@section{Step 2: Build The Project}

Run @litchar{cd xiden} to enter the source directory. We will now build
the executable and offline documentation.

If you have GNU Make installed, then just run @litchar{make}. Otherwise,
open @tt{Makefile} and run the commands defined by the @tt{build} target.

If successful, then @|binary| should now work as a command in your shell. If
not, check your @tt{PATH} to make sure that it includes the directory where
@tt{raco setup} creates launchers on your disk.

Alternatively, you can access the same command line interface using the
@litchar{raco zcpkg} (“zero collection package”) command.
