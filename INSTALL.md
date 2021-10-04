# Setup

After installing Denxi, you will have a `denxi` command available for
your system.

First, make sure the following programs are available in your `PATH`.

* SQLite 3.24.0+. Verify with `sqlite3 -version`

* Racket 7.0+. Verify with `racket -v`

> To build from source, clone or download Denxi from
> [GitHub](https://github.com/zyrolasting/denxi.git) and run `make` in the
> source directory.

Next, run `raco pkg install denxi`. If this fails, then make sure you do
not have a conflicting version installed.

The `denxi` command should now be available.
