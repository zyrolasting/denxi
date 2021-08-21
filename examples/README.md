# Xiden Examples

If you've finished the guide, then you've been sent here to
practice. You'll see what launchers and package definitions can
do. Every now and then an example will ask you to look up something
with `raco docs`. Do so. It will give you experience with the
reference material.  Once you can work with the reference, you will
know how to write your own examples. :)


## After the Guide
1. [From Guide](./from-guide): The example from the guide, repeated
   for convenience and for comparison to later
   examples. (@zyrolasting)
1. [Workspaces](./workspaces): How Xiden stores state. (@zyrolasting)
1. [Input and Output Selection](./input-output-selection): Package
   definitions can have any number of inputs, but outputs decide what
   make it to disk. (@zyrolasting)
1. [Integrity Checking](./integrity-checking): Verify that content is
   correct.  (@zyrolasting)
1. [Signature Checking](./signature-checking): Verify that content
   came from someone you trust.  (@zyrolasting)
1. [Versioning](./versioning): Add version information to package
   definitions. (@zyrolasting)
1. [Output Conflicts](./output-conflicts): Deal with conflicts in
   installed outputs. (@zyrolasting)
1. [Input Proliferation](./input-proliferation): Dealing with uncached
   inputs. (@zyrolasting)


## Package Definitions
1. [Input Overriding](./input-overriding): Substitute inputs at
   runtime. (@zyrolasting)
1. [Abstract Inputs](./abstract-inputs): How to work with inputs that
   only have names.  (@zyrolasting)
1. [Abstract Outputs](./abstract-outputs): How to work with outputs
   that only have names.  (@zyrolasting)
1. [Artifact Deterministim](./determinism): How to guarentee that an
   artifact is always useable.  (@zyrolasting)
1. [Allow Racket Versions](./allow-racket-versions): Ask Xiden to
   process a definition depending on the current Racket
   version. (@zyrolasting)
1. [Generated Racket Bindings](./generated-racket-bindings): Fix a
   problem with non-`eq?` generated bindings that plagues Racket
   programs distributed on PLaneT, and sometimes `raco pkg`. (@zyrolasting)
1. [Self-hosted Xiden](./self-hosting): Install Racket with it's own
   copy of Xiden. (@zyrolasting)


## Launchers
1. [The `fetch` Command](./fetch-command): Using the default `fetch`
   command. (@zyrolasting)
1. [Maximum Trust](./maximum-trust): What's the _worst_ way to use
   Xiden? (@zyrolasting)
1. [Cryptography Backends](./cryptography-backends): Selecting a back
   end for cryptographic operations. (@zyrolasting)
1. [Racket Installation Manager](./racket-installation-manager):
   Install and manage multiple versions of Racket. (@zyrolasting)
