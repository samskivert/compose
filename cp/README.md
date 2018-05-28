# Compose Compiler / Editor Mark Zero Prime

This is the bootstrap compiler and editor for Compose: `CE0'`.

After making a bunch of progress on [C0] (the first bootstrap compiler for Compose), I came to some
realizations:

1. The boostrap version of the language was evolving into something semantically similar to
   [PureScript], but with a less rigorous approach to handling side-effects. I may still make
   different choices for side-effect management in the final language (versus approaches used by
   PureScript or Haskell, for example), but it will be a good exercise for me to write C0 and E0 in
   a pure language to confirm (or refute) my notions about the right approach to differentiating
   pure from impure code.

2. C0 started with a Scala-inspired syntax rather than a Haskell-inspired syntax, mainly for ease
   of parsing. My hope was that the resulting AST could be easily evolved into the final AST once I
   started working on the projectional editor (which I didn't want to write in Scala and then have
   to completely rewrite in Compose later). But I think that writing the editor in PureScript might
   be close enough to my target AST (and semantics) that I will be able to easily port an `E0`
   implementation to Compose, once it is fully featured enough to self-host.

3. The projectional editor and associated programmer support mechanisms are the most important and
   differentiating thing about Compose, so jumping right into exploring and refining my ideas there
   is a benefit that outweighs the cost of having to do some extra rewriting later.

So I'm starting afresh on `C0'` (compiler) and `E0` (editor), written in PureScript. Whee!

## Roadmap

TBD

[C0]: ../c0
