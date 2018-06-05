# Compose Programming Language

*Compose* is an experimental programming language. It aims to be:

* teachable: minimize boilerplate and incidental complexity
* scalable: work well from one liners to million line systems
* modern: try not to ignore the myriad lessons we've learned in PL over the last fifty years
* powerful: provide high-level and meta-programming capabilities
* precise: sane semantics are king, minimize concessions to legacy

Compose also prioritizes a lot of things other programming languages don't even address as
problems. For more details, read the [manifesto] and [FAQ].

## Status

Work originally started on [C0](c0), the Compose Mark Zero compiler, written in [Scala]. The
original plan was to make a batch compiler for a language similar to the final form of Compose
(though with different syntax) and then use that to build an editor/compiler combo, which would
work on the real language. But at some point I decided that was a bad idea.

Next, I started on an editor/compiler combo written in [PureScript]: [PCE0](pce0). This allowed me
to start tackling some of the thorny editor problems earlier, which I was comfortable with, having
gotten so far down the road on the type system and compiler. But PureScript turned out to be too
annoying to work with (for reasons detailed on the PC0 page). So I took off and nuked everything
from orbit again.

Presently, I am working on an editor/compiler combo written in [Reason]: [RCE0](rce0). The goals
here are the same as for PC0: start making progress on the major editor challenges and build the
compiler slowly along the way. Hopefully Reason and I will get along better than I did with
PureScript.

## Contact

Compose is being created by Michael Bayne <mdb@samskivert.com>. Feel free to contact me if you like
to talk about the utter insanity of trying to foist another programming language on the world.

[FAQ]: https://samskivert.github.io/compose/faq/
[PureScript]: http://www.purescript.org/
[Reason]: https://reasonml.github.io/
[Scala]: https://www.scala-lang.org/
[manifesto]: https://samskivert.github.io/compose/posts/manifesto/
