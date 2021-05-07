#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "setup"]{Setup}

This section covers how to install @tt{xiden} on your system.


@section{Required Programs}

First, make sure the following programs are available in your @tt{PATH}.

@itemlist[
@item{SQLite 3.24.0+. Verify with @litchar{sqlite3 -version}}
@item{Racket 7.0+. Verify with @litchar{racket -v}}
@item{OpenSSL 0.9.8+. Verify with @litchar{openssl version}}
]


@section{Install from Racket's Default Package Catalog}

Xiden is available as a vanilla Racket package on the default
catalog. Run @litchar|{raco pkg install xiden}| to install it, and
you're done.

If this fails and you can connect to the catalog fine, then these are
the likely reasons:

@itemlist[

@item{Your Racket installation may contain a package that defines a
conflicting @litchar|{xiden}| collection. That package can either be a
different edition of Xiden, or any package snuck into your
installation designed to create that conflict.}

@item{Xiden's package tracks the latest release and is
subject to backwards-incompatible changes as a
@hyperlink["https://sagegerard.com/new-racket-pkg-releases.html"]{matter
of policy.} In this case, uninstall the old version of Xiden and
review the
@hyperlink["https://github.com/zyrolasting/xiden/blob/master/CHANGELOG.md"]{change
log}.}

]


@section{Install from Source}

This method builds Xiden from your desired version of its
source code.

@subsection{Step 1: Get the Code}

The goal of this step is to get a directory named @tt{xiden} somewhere on
your disk.

You can either use Git or download an archive.


@subsubsection{Method: Use Git}

If you wish to use Git, you can clone Xiden's repository. If
you use SSH, then run @litchar|{git clone
git@github.com:zyrolasting/xiden.git}|.  Or, if you use HTTPS, then
run @litchar|{git clone https://github.com/zyrolasting/xiden.git}|.
If you know what you are looking for, check out the commit you want to
use.  Otherwise, move on to Step 2.


@subsubsection{Method: Download Archive}

If you do not have Git, then you can download a ZIP file
of the latest source code from the following link:

@centered[@hyperlink["https://github.com/zyrolasting/xiden/archive/master.zip"]{https://github.com/zyrolasting/xiden/archive/master.zip}]

If you know what you are looking for, replace @litchar{master} with
the commit reference you want to use. Once the archive is on your
disk, extract its files.

@bold{You should have a directory named @tt{xiden} with a bunch of
Racket modules in it}. If you have a different directory name, or if
the directories are nested, then make sure to reorganize the
directories accordingly.


@subsection{Step 2: Build The Project}

Enter the source directory created from Step 1. We will now build the
executable and offline documentation. If you see @tt{Makefile} in your current
directory, then you are in the right place.

If you have GNU Make installed, then just run @litchar{make}. Otherwise,
open @tt{Makefile} and run the commands defined by the @tt{build} target.

If successful, then the @tt{racket -l xiden} command will run without
error.
