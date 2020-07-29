A flexible package manager for Racket.


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

Yes, and it's fine. `zcpkg` draws different boundaries on what
constitutes a package, and how it impacts a surrounding system.

To borrow [Sam Boyer's terms][boyer], `raco pkg` is an **Language Package
Manager**, or LDM.  It fetches and builds packages of Racket source
code. `raco pkg` further presumes that Racket packages define at least
one collection to include in a Racket installation, which creates
scenarios specific to Racket.

However, a Racket program is not fully "contextualized" (for lack of a
better term) until it is launched. To customize around things like
package conflicts or search paths, you need to create custom executables
and/or tethered Racket installations.

`zcpkg` started when I wondered what would happen if a Racket package
defined zero collections (The command `zcpkg` is short for
"Zero-Collection Package").  Removing collections from the equation is
a radical departure that severs the relationship between an arbitrary
group of files and a Racket installation. No `raco setup`, no
collection paths. For that reason, `zcpkg` concerns itself with
(re)producing files that extend a given Racket installation without
modifying it. From that end, you could call it a **Project/Application
Dependency Manager**, or PDM. There are elements of system-level
dependencies at work, but that's out of scope for this section.

All you need to know up front is that `zcpkg` handles cases to
compliment `raco pkg`.


[boyer]: https://medium.com/@sdboyer/so-you-want-to-write-a-package-manager-4ae9c17d9527


## Project Status

Unfinished. The server-side remains incomplete. The client side is
useable, but subject to change.

My goal is to get the project to an alpha status by August 1st,
but that is unlikely in the minimal Racket case. Long term, I would
hope this project will be a Racket-flavor of Guix or Nix.


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
