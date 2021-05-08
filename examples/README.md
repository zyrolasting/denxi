This directory contains example programs that use Xiden.

Each example is self-contained. Directory names are numbered to
indicate the recommended reading order, starting with `00`.  You will
need Xiden installed, so be sure you've followed the [setup
instructions][setup].


# Conventional Files

Each directory may contain the following files:

* `README.md`: Instructions for using the example
* `program.rkt`: a hypothetical program that does not function without dependencies.
* `defn.rkt`: the primary package definition used to fulfil dependencies.
* `launcher.rkt`: a launcher used to configure and run Xiden.


# Resetting an Example

Instructions may tell you to "reset" an example, if needed.  This is
because the commands in the examples will create files and symbolic
links.  To restore the initial state of the example, delete the
symbolic links and run `xiden gc`.


# Any Issues?

The instructions for an example will tell you what you can expect to see.
If you get any output or change that is not consistent with the instructions,
please [open an issue][]


[open an issue]: https://github.com/zyrolasting/xiden/issues
[setup]: https://docs.racket-lang.org/xiden-guide/setup.html?q=xiden%20guide
