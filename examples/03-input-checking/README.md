This example exposes you to trust violation errors under a zero trust configuration.

We're going to try installing several packages that all use the same
input data, with varying levels of information for Xiden to verify.
We will incrementally consent to different things until we finally
authenticate an input.

1. Run `xiden do +a no-integrity.rkt`. It will fail because Xiden rejected the input for lacking integrity info.
2. Run `xiden do +a no-signature.rkt`. It will fail because Xiden rejected the input for lack of trust in the crypto hash function.
3. Add trust to the crypto hash function to the plugin in `xiden-workspace/etc/xiden.rkt`.
4. Run `xiden do +a no-signature.rkt` once more. It will fail because Xiden rejected the input for lacking a signature.
5. Run `xiden do +a defn.rkt`. It will fail because you didn't say you trusted the author's (Sage Gerard's) public key.
6. Copy the `(integrity ...)` expression to your clipboard from the output of Step 5.
7. Open `xiden-workspace/etc/xiden.rkt` and follow the instructions from the output of step 5.
8. Run `xiden do +a defn.rkt`. It will now work.

We didn't do this work in earlier examples because those examples shut
off Xiden's safeguards. Some examples will do that to keep the code
brief if the input is both local and harmless, but you should not turn
off the safeguards without good reason. Not authenticating inputs is
equivalent to allowing arbitrary data on your system, which can lead
to arbitrary code execution.
