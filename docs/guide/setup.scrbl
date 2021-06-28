#lang scribble/manual

@require["../shared.rkt"]

@title[#:tag "setup"]{Setup}

After installing Xiden, you will have a @litchar{xiden} command
available for your system.

First, make sure the following programs are available in your @tt{PATH}.

@itemlist[
@item{SQLite 3.24.0+. Verify with @litchar{sqlite3 -version}}
@item{Racket 7.0+. Verify with @litchar{racket -v}}
]

@margin-note{To build from source, clone or download Xiden from
@hyperlink["https://github.com/zyrolasting/xiden.git"]{GitHub} and run
@litchar{make} in the source directory.}

Next, run @litchar|{raco pkg install xiden}|. If this fails, then
make sure you do not have a conflicting version installed.

The @tt{xiden} command should now be available.
