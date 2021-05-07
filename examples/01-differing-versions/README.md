`program.rkt` imports modules with the same functionality, but Racket
sees them as providing different bindings. We can use Xiden to control
the bindings Racket sees without changing `program.rkt`.

1. Run `racket launch.rkt do +a defn.rkt` in this directory. You should see a symlink called `default` appear.
2. Run `racket program.rkt` again. You should see it print `different`, indicating the bindings are different.


# Method: Input Overrides

An input override is when you tell Xiden to replace input expressions
in some packages by name. We can use this to force the `"v2.rkt"`
input to use the same source as the `"v1.rkt"` input.

1. Run `racket launch.rkt do +a defn.rkt +o '#rx""' '(input "v2.rkt" (sources (from-file "sources/v1.rkt")))'`
2. Run `racket program.rkt` again. It will print `same`.

The `'#rx""'` is read as a blank regular expression in Racket's
notation. That trivially matches any package, which makes the package
we use eligible for overriding.


# Method: Duplicate Inputs

Xiden will only store one copy of exact content and issue links to
that copy to save space. You can leverage this by providing duplicate
data.

1. Reset the example
2. Run `cp sources/v1.rkt sources/v2.rkt`
3. Run `racket launch.rkt do +a defn.rkt`
4. Run `racket program.rkt` again. It will print `same`.
