Racket programs that fetch dependencies struggle with generated
bindings, because their values are never `eq?` when you expect them to
be.

`program.rkt` imports modules with the same functionality, but Racket
sees them as providing different bindings. We can use Denxi to control
the bindings Racket sees without changing `program.rkt`.

1. Run `racket launch.rkt do +a defn.rkt` in this directory. You should see a symlink appear.
2. Run `racket program.rkt`. You should see it print `different`, indicating the bindings are different.


# Method: Input Overriding

Here we use an input override to force the `"v2.rkt"` input to use the
same source as the `"v1.rkt"` input.

```
override='(input "v2.rkt" (artifact (file-source (from-file "sources/v1.rkt")) #f #f))'
match_everything='#rx""'
racket launch.rkt do +a defn.rkt +o "$match_everything" "$override"
```

After performing the above setup, run `racket program.rkt` again. It
will print `same`.


# Method: Duplicate Inputs

Denxi wil not duplicate inputs, so you can leverage its cache such
that the same files of two different names result in links pointing to
the same Racket module file.

```
cp sources/v1.rkt sources/v2.rkt
racket launch.rkt do +a defn.rkt`
```

Run `racket program.rkt` again. It will print `same`.
