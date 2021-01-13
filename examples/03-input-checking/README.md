This authenticates an input signed using the author's private key.
We're going to try installing several packages that all use the same
input data, with varying levels of information for Xiden to verify.

1. Run `xiden do +a no-integrity.rkt`. It will fail because Xiden rejected the input for lacking integrity info.
2. Run `xiden do +a no-signature.rkt`. It will fail because Xiden rejected the input for lacking a signature.
3. Run `xiden do +a defn.rkt`. It will fail because you didn't say you trusted the public key.
4. Copy the `(integrity ...)` expression to your clipboard from the output of Step 3.
5. Open `xiden-workspace/etc/xiden.rkt` and follow the instructions in the comment.
6. Run `xiden do +a defn.rkt`. It will now work.

We didn't do this work in earlier examples because those examples shut
off Xiden's safeguards. Some examples will do that to keep the code
brief if the input is both local and harmless, but you should not turn
off the safeguards without good reason. Not authenticating inputs is
equivalent to allowing arbitrary data on your system, which can lead
to arbitrary code execution.
