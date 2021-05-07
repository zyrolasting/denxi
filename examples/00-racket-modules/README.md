This is an introductory example that says `Hello!` using one dependency.

In this case, the dependency is expressed directly in the package
definition file, which we trust implicitly in the launcher. Implicit
trust is a bad habit we will break in future examples.

1. Run `racket program.rkt`. You will see an error that the module cannot find `dependencies/hello.rkt`.
2. Run `racket launch.rkt do +a defn.rkt`. You will see status messages.
3. Run `racket program.rkt` again. You will see `Hello!`
