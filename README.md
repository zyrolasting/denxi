A better package manager for Racket.

## Project Status

Unfinished. The server-side remains incomplete. The client side is
useable, albiet subject to change. Another branch, `racket-minimal` is
ongoing to adapt the existing code for use on a minimal Racket
distribution.

My goal is to get the project to an alpha status by August 1st,
but that is unlikely in the minimal Racket case.


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

## Setup

First, make sure the following programs are available in your search paths.

* Racket 7.0+. Verify with `racket -v`.
* OpenSSL 0.9.8+. Verify with `openssl version`.

Next, build the project using `make`.

```console
git clone ...
cd zcpkg
make
```

If everything worked, you should have HTML documentation in a `doc`
subdirectory and a new executable called `zcpkg`. If you see an error
relating to a missing Racket dependency, then you might be using a
trimmed down Racket installation. In that case, try `make racket-deps
&& make`.
