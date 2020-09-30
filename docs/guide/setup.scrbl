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
@item{GNU Make v4.0.0+. Verify with @litchar{make -v}}
]

@section{Step 1: Get the Code}

There are currently no pre-built binaries for @|project-name|, so we need to
build from source. The goal of this step is to get a directory named
@binary somewhere on your disk.

You can either use Git or download an archive.

@subsection{Option: Use Git}

If you wish to use Git, you can clone the default branch from @|project-name|'s
repository. If you use SSH, then run @litchar|{git clone
git@github.com:zyrolasting/xiden.git}|.  Or, if you use HTTPS, then run
@litchar|{git clone https://github.com/zyrolasting/xiden.git}|

Either command will leave a @binary directory in your working directory,
and you can move on to Step 2.

@subsection{Option: Download Archive}

If you do not have Git, then you can download a ZIP file
of the latest source code from the following link:

@centered[@hyperlink["https://github.com/zyrolasting/xiden/archive/master.zip"]{https://github.com/zyrolasting/xiden/archive/master.zip}]

You can then extract the archive using whatever tool you prefer. Just make
sure that the source files are inside of a directory named @|binary|.

@section{Step 2: Link Collection}

Do not change directories yet. Before you go into the @binary directory, run
@litchar{raco link xiden}. This will tell your Racket installation to create
a collection of the same name using that directory.


@section{Step 3: Build The Project}

Finally, run @litchar{cd xiden} and then @litchar{make}. If successful, you
will see an executable file called @binary appear in your working directory.
You will also see a @tt{html} directory appear. That holds offline
documentation.  @tt{html/guide/index.html} is the home page for the guide you
are reading now.  @tt{html/reference/index.html} is the home page for the
reference material.

Move @binary (the executable) anywhere you'd like, but make sure that the
directory holding @binary appears in your @tt{PATH} so that you can run it
easily in your shell.
