A functional package manager for Racket.


## Benefits

* You can install the _exact_ code you asked for.
* You can make links to the _exact_ files you need in your projects.
* You can use packages that would otherwise conflict in the same Racket installation.
* You can introduce package code starting from _zero trust_.
* You can publish breaking changes whenever you want.
* You can organize your files however you want.
* You can use your own package namespace.
* You can define deliverables with more creative freedom, outside of the PLT's recipes.
* You can strictly separate third-party code from your Racket installation.


## Tradeoffs

* Higher storage requirements
* A different workflow


## But we already have `raco pkg`

Yes, and it's fine. `xiden` complements `raco pkg`, so it is not meant
to be fully redundant. But both projects are package managers, so some
functional overlap exists.

So what's different? In brief: `raco pkg` has side-effects on the
Racket installation that runs it. `xiden` does not. This means `xiden`
does not create collections in an existing Racket installation. You
cannot directly target Racket modules in these _zero-collection packages_
using `raco setup` or collection paths. The benefit is that `xiden` behaves
more like [Guix][], in that packages are built deterministically and
in a fault-tolerant manner. When you want to use individual packages
or the files within them, you can request links to those files.

[Guix]: https://guix.gnu.org/

## Setup

First, make sure the following programs are available in your search paths.

* SQLite 3.24.0+. Verify with `sqlite3 -version`.
* Racket 7.0+. Verify with `racket -v`.
* OpenSSL 0.9.8+. Verify with `openssl version`.

Next, build the project using `make`.

```console
git clone ...
cd xiden
make
```

If everything worked, you should have HTML documentation in an `html`
subdirectory and a new executable called `xiden` in the working
directory.


## Release Status

| Milestone   | Status                                                                |
| ----------- | --------------------------------------------------------------------- |
| Alpha       | [Released](https://github.com/zyrolasting/xiden/releases/tag/alpha)   |
| Closed Beta | [Ongoing](https://github.com/zyrolasting/xiden/milestone/1)           |
| Open Beta   | [Not started](https://github.com/zyrolasting/xiden/milestone/2)       |
| Production  | [Not started](https://github.com/zyrolasting/xiden/milestone/3)       |
