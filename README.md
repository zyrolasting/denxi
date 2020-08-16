A flexible package manager for Racket.

## Goals

* Smooth the learning curve for dependency management
* Give users as much control over distribution as possible
* Support secure, reliable code exchange.


## Benefits

* You can install the _exact_ code you asked for.
* You can make links to the _exact_ versions of packages you need in your projects.
* You can use packages that would otherwise conflict in the same Racket installation.
* You can introduce package code starting from _zero trust_.
* You can publish breaking changes whenever you want.
* You can organize your files however you want.
* You can use your own package namespace.
* You can define deliverables with more creative freedom, outside of PLT's recipes.
* You can strictly separate third-party code from your Racket installation.


## Tradeoffs

* Dependencies are installed side-by-side ("SxS", for the hopelessly curt). This means added disk usage.
* Dependency management is less opinionated, hence more involved if you have highly-specific needs.
* Package development does not involve `raco setup` or `raco pkg`. The workflow is different.


## Wait, we already have `raco pkg`

Yes, and it's fine. `zcpkg` complements `raco pkg`. They are not meant
to be fully redundant.

Because both projects are package managers, some functional overlap is
expected. So what's different? To borrow [Sam Boyer's terms][boyer],
`raco pkg` is a **Language Package Manager**, or LPM.  It fetches and
builds packages of Racket source code. `raco pkg` presumes that Racket
packages define at least one collection to include in a Racket
installation. Additionally,
[pkgs.racket-lang.org](https://pkgs.racket-lang.org) (the default
catalog) remembers only one artifact per registered
package. Altogether, what a package installation does to your system
depends on the exact state of your Racket installation, and what
happens to be on a catalog at the time.

The command `zcpkg` is short for "Zero-Collection Package," meaning
the program manages packages that define no collections. This is just
marketing, since I expect Racket programmers can reason about a
zero-collection package as part of the casework about package
management: The only difference that matters is that `zcpkg` does not
have a side-effect on a Racket installation in the first place. Not
defining collections is a consequence of that restriction.

Removing collections from the equation severs the relationship between
an arbitrary group of modules and a Racket installation. Meaning, you
cannot directly target these files using `raco setup` or collection
paths. The benefit is that you are free to integrate the files as you
wish. For that reason, `zcpkg` concerns itself with (re)producing
files as a **Project/Application Dependency Manager**, or
PDM. System-level dependencies matter too, and `zcpkg` can be
leveraged to define an OS distribution if you care to do that.

[boyer]: https://medium.com/@sdboyer/so-you-want-to-write-a-package-manager-4ae9c17d9527


## Setup

First, make sure the following programs are available in your search paths.

* SQLite 3.24.0+. Verify with `sqlite3 -version`.
* Racket 7.0+. Verify with `racket -v`.
* OpenSSL 0.9.8+. Verify with `openssl version`.

Next, build the project using `make`.

```console
git clone ...
cd zcpkg
make
```

If everything worked, you should have HTML documentation in an `html`
subdirectory and a new executable called `zcpkg`. If you see an error
relating to a missing Racket dependency, then you might be using a
trimmed down Racket installation. In that case, try `make racket-deps
&& make`.


## Release Status

| Milestone   | Status                                                                |
| ----------- | --------------------------------------------------------------------- |
| Alpha       | [Released](https://github.com/zyrolasting/zcpkg/releases/tag/alpha)   |
| Closed Beta | [Ongoing](https://github.com/zyrolasting/zcpkg/milestone/1)           |
| Open Beta   | [Not started](https://github.com/zyrolasting/zcpkg/milestone/2)       |
| Production  | [Not started](https://github.com/zyrolasting/zcpkg/milestone/3)       |
