If you've finished Xiden's guide, use this directory to practice it by
example. This will teach you more about the problem domain and how to
read Xiden's reference material.

In the event you ran `ls` and gulped, **you do not need to read all of
the examples.** Just do the [required reading](#required-reading),
then use the remaining examples to supplement your knowledge or look
up a way to handle something. You can consider yourself done whenever
you feel comfortable being left alone with the reference
documentation, and can look things up from here when convenient.

Each example is self-contained. Just open one and read the code.
You'll pick up on the patterns as you go, but remember the _main_
pattern: **Everything boils down to launchers and package
definitions.**  From there we can handle more interesting scenarios
and use cases.

The examples will start with baby steps, but once you are done with
required reading you will need to lean more on reference
documentation. Some examples may even ask you to edit them. There will
always be enough contextual clues around for you to see your options
and the rules that apply to a program. This will (hopefully) prevent
you from assuming that certain patterns always hold. That's not an
assumption you want to have about software distribution.


## Required Reading
1. [From Guide](./from-guide): The example from the guide,
   repeated for convenience and for comparison to later examples. (@zyrolasting)
1. [Workspaces](./workspaces): How Xiden stores state. (@zyrolasting)
1. [Integrity Checking](./integrity-checking): Verify that content is correct.  (@zyrolasting)
1. [Signature Checking](./signature-checking): Verify that content came from someone you trust.  (@zyrolasting)
1. [Versioning](./versioning): Add version information to package definitions. (@zyrolasting)
1. [Output Conflicts](./output-conflicts): Deal with conflicts in installed outputs. (@zyrolasting)
1. [Input Proliferation](./input-proliferation): Dealing with uncached inputs. (@zyrolasting)
1. [Input Overriding](./input-overriding): Substitute inputs at runtime. (@zyrolasting)
1. [Maximum Trust](./maximum-trust): What's the _worst_ way to use Xiden? (@zyrolasting)


## Launchers
1. [Racket Installer](./racket-installer): This launcher downloads and
   executes Racket installers. (@zyrolasting)
1. [Python Downloader](./python-archive): Download and extract Python
   source code. (@zyrolasting)
1. [Self-hosted Xiden](./self-hosting): Install Racket with it's own
   copy of Xiden. (@zyrolasting)
1. [The `fetch` Command](./fetch-command): Using the default `fetch`
   command. (@zyrolasting)


## Package Definitions
1. [Abstract Inputs](./abstract-inputs): How to work with inputs that only have names.  (@zyrolasting)
1. [Abstract Outputs](./abstract-outputs): How to work with outputs that only have names.  (@zyrolasting)
1. [Artifact Deterministim](./determinism): How to
   guarentee that an artifact is always useable.  (@zyrolasting)
1. [Self-hosted Installation](./self-hosting): Install Racket with its own copy of Xiden. (@zyrolasting)
1. [Allow Racket Versions](./allow-racket-versions): Ask Xiden to process a definition depending on the current Racket version. (@zyrolasting)
1. [Generated Racket Bindings](./generated-racket-bindings): Fix a
   problem with non-`eq?` generated bindings that plagues Racket
   programs distributed on PLaneT, and sometimes `raco pkg`.
