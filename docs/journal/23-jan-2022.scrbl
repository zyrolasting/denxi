#lang reader "../scribble.rkt"

@title{23 January 2022}
@by-slg

Bah, I've done it again and increased the scope of my refactoring.  I
just really love deleting code.  Nothing feels better than realizing I
was overcomplicating something, because that means I get to replace it
with a leaner approach. When you do it right, work is never lost, only
reframed. If only I could realize such things more quickly.

There's a few big changes.


@ssection{CLI, out!}

I realized I didn't need a CLI. In my documentation and examples, I
immediately point users to writing their own launchers, or entry
points. If that's the case, there isn't much use for calling attention
to the default CLI except as a means to distribute a launcher using a
long command. A long command that people would share to simplify
things, when they could just use a launcher. The default launcher acts
as a thin wrapper for what would already act as shared functionality
for users, and removing the default CLI does not curtail one's ability
to distribute a launcher with a vanilla Racket program. There is a
baby in the bathwater that leads to a useful
@tech/reference{parameterization}, but I can salvage it with a leaner
approach that accounts for Denxi settings and vanilla Racket
parameters.

The other benefit to this change is that it simplifies Denxi's formal
identity to that of a library.


@ssection{Goodbye Package Definitions, Hello Catalogs}

The name “package definition” is not entirely accurate. A package
definition describes a set of possible packages as opposed to exactly
one. Since a user can pick and choose artifacts by name in these
definitions, it made sense to use the term “catalog” instead. By
replacing @litchar|{#lang denxi}| with @litchar|{#lang
denxi/catalog}|, I can still emphasize an artifact- and
dependency-centric view without calling attention to packages that do
not actually exist. Rather than view a package definition and package
in a manner similar to a program and a process, I view a package as a
dynamic entity built from selections in the catalog. This more
accurately captures what's happening. What makes things tricky is how
to model the dependency relationships when the user ultimately
controls the name of each artifact. If names across catalogs conflict,
it's up to the user to resolve what the names mean. I have not decided
the best approach for this yet.


@ssection{State is an Idea}

The state model now abstracts over storage mediums. Denxi originally
took a file-centric view to state, but it now has an interface for
state that now points to memory in a prototyping context. No more file
I/O in unit tests, no more SQLite dependency, and no more
workspaces. All remain necessary for implementations of the interface,
but none offered details relevant to Denxi's critical invariants. The
implementations are best viewed as extensions to Denxi to leverage in
a launcher.


@ssection{Leaner Guide and White Paper}

The source code for the guide has been consolidated into one document.
Not needing queries or many package terms made things easier to think
about.

The biggest simplification came from reframing Denxi's value in terms
of shipping and receiving. I don't ask readers to focus about package
definitions, I ask them to focus on an analogy of ordering from a
catalog, getting a package at the door, and responding when the
package is missing necessary contents. It's also easier to explain
Denxi's user-centric consent model as a way to make sure the delivery
driver stays outside of a recepient's house.


@ssection{Denxi's Dependencies Derail Demonstrations}

It's weird to write a dependency management solution with
dependencies. It makes me feel like I want to be done so I can use my
program for itself. It's better to lose the need for dependencies that
are not available in @litchar{racket-minimal}.  SQLite3 is, from what
I recall, but that's already gone. There should also be no use of GNU
Make, no RackUnit, and, with luck, no Scribble.

RackUnit is a heavy dependency for @litchar{minimal-racket}, since it
includes utilities specific to DrRacket. That in turn means
GUI-specific dependencies that are decidedly @italic{not}
minimal. Rolling my own assertion styles is hardly an issue. The issue
is porting the test code in a structured way. For that I am thankful
for S-expressions.

Scribble is an odd case. It is the only reason why Denxi is still
organized like a Racket package. I also feel increasingly
uncomfortable with the code duplication it seems to demand, since
forms like @racket[defproc] always seem to want a variation of the
same data I write in @racket[contract-out]. I also don't understand
this idea of documents as programs. Programs are already documents.
Every time I write Scribble, I feel like I'm on a powerful horse
staring at the back of a cart.

I loathe jumping between files in different directories to keep logic
in sync, and Scribble is a pain about that. Reminds me of Java,
almost. It also serves as a reminder of how I'm failing to abandon
Racket's optimistic ideas about the use of collections for
third-parties. An entry of this diary still appeared in the shared
online documentation for Racket because I failed to explicitly omit it
from the search index.  The fact I can get free hosting for this text
because this community runs on the honor system... I still don't see
eye-to-eye with the community about this. It reminds me why I want out
of their ecosystem, and why I started Denxi. I'm just not this
trusting.


@ssection{Parameters are Too Easy to Misuse}

Racket parameters are fancy globals that help you avoid passing more
arguments to functions.

I'm not the first to say so, but this is the first time I feel I can
be emphatic about how much parameters harm program design when they
are misused.

Some of Denxi's tests failed when I ran them using
@litchar{racket-mode}, as opposed to @litchar{raco test}. The
@hyperlink["https://github.com/racket/racket/issues/4131"]{root cause}
is that Racket's @racket[exn->string] assumes that
@racket[error-display-handler] writes to
@racket[current-error-port]. At the time, @litchar{racket-mode}
directs all output to @racket[current-output-port] for its REPL
buffers, making @racket[exn->string] unconditionally return
@racket{""}. When calling @racket[exn->string], you have to remember
to update the @tech/reference{parameterization} in addition to passing
an argument. But that's not good enough, because @litchar{racket-mode}
preempts the code it runs.  You can't just capture the value of
@racket[(error-display-handler)] on instantiation; you'll just get
@litchar{racket-mode}'s. This was patched (Thanks, Greg!), but that
didn't change my now growing distrust of parameters. If Racket's core
team is allowing the mistake that is @racket[exn->string] to stand,
then who knows what else my parameterized code will do?

Unlike most formatting functions, @racket[exn->string] does not always
do what it's name says. It appears it will never accept a port as an
argument that defaults to @racket[(error-display-handler)], in the
same way that printer functions accept a port that defaults to
@racket[(current-output-port)]. The Racket team settled on a warning
in the documentation instead of adding an optional argument. I
understand the value of backwards-compatibility, if that's the issue.
But as you can tell, I'm not married to the idea. I certainly wouldn't
use it as a defense for the current state of
@racketmodname[racket/exn].

I no longer wish to allow my Racket code to use parameters unless I
know I'm in complete control of the parameterization.  Problem is, I
can't see a way to claim that I do.

The problem now is that if I remove parameters, my function signatures
would bloat. I could take the corporate programmer approach and
dedicate structure types to each function as if it were a new API.
Problem is, that's an omnipresent extra step that bloats the code in a
different way. It takes a different instinct to know what formals of a
function signature should appear in a structure type, especially with
generated bindings.

I'm considering a new monadic type that carries configuration
information.  Right now I use monomorphic returns with a single
subprogram type. That type cobbles together semantics from State and
Maybe monads. The subprogram type served me well, but I can't keep
adding it. It's already a little hard to follow, and monads are hard
enough.
