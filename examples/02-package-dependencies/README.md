This example shows how package definitions can depend on other package
definitions.

1. Run `xiden do +a defn.rkt`
2. Run `racket program.rkt`. It will print `It works!`

Depending on a package means naming another package definition as an
input. Once that input is resolved, the dependent package
programmatically installs the dependency.
