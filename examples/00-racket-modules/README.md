This is a simple example that says `Hello!` using one dependency.

1. Run `racket program.rkt`. You will see an error that the module cannot find `dependencies/hello.rkt`.
2. Run `xiden do +d dependencies defn.rkt` in this directory. You will some output.
3. Run `racket program.rkt` again. If you get the same error, please share the output of Step 2 in an issue. If you see `Hello!`, then it works.
