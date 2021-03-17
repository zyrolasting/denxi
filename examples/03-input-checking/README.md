This example walks you through how to affirm trust in specifics
starting from a zero trust configuration.

1. Run `xiden do +a defn.rkt`. It will fail because you didn't consent to use the given cryptographic hash function. It will tell you the name (e.g. `sha384`).
2. Open `xiden-workspace/etc/xiden.rkt` and add the function name from Step 1. A comment in that file will tell you where to put it.
3. Run `xiden do +a defn.rkt` again. It will fail because you didn't say you trusted the public key meant for this tutorial.
4. Copy the `(integrity ...)` expression to your clipboard from the output of Step 3.
5. Open `xiden-workspace/etc/xiden.rkt` and add the `(integrity ...)` expression where instructed.
6. Run `xiden do +a defn.rkt`. It will now work.

We didn't do all this in earlier examples because those examples shut
off Xiden's safeguards. Harmless demos are one thing, but you should
not turn off the safeguards without good reason.

You'll only repeat one of these steps whenever you encounter a new,
untrusted instance of data.
