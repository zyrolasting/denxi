This is an introductory example that says `Hello!` using one dependency.

In this case, the dependency is expressed directly in the package
definition file, which we trust implicitly in the configuration shown
in `xiden-workspace/etc`. Implicit trust is a bad habit we will break
in future examples.

1. Run `racket program.rkt`. You will see an error that the module cannot find `dependencies/hello.rkt`.
2. Run `xiden do +a defn.rkt`. You will see status messages.
3. Run `racket program.rkt` again. You will see `Hello!`
