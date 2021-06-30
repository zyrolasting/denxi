# Setup

After installing Xiden, you will have a `xiden` command available for
your system.

First, make sure the following programs are available in your `PATH`.

* SQLite 3.24.0+. Verify with `sqlite3 -version`

* Racket 7.0+. Verify with `racket -v`

> To build from source, clone or download Xiden from
> [GitHub](https://github.com/zyrolasting/xiden.git) and run `make` in the
> source directory.

Next, run `raco pkg install xiden`. If this fails, then make sure you do
not have a conflicting version installed.

The `xiden` command should now be available.
