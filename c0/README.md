# Compose Compiler Mark Zero

This is the (wildly incomplete) bootstrap compiler: `C0`.

Compose has a substantial chicken and egg problem in that it is not compiled from text files nor
edited via a normal text editor, but we don't yet have an editor that can create Compose ASTs
interactively, an analyzer to type check them, a compiler to compile them, etc.

I don't want to write an entire compiler **and** an entire editor in some other language and then
rewrite them all in Compose. So I'm writing `C0` which will compile "Compose-like" code from text
files. Then I will write `E0` (the first iteration of the editor) in `C0`, and finally write the
"real" compiler and editor for Compose.

So try not to draw too many conclusions from how `C0` looks or works. I aim for it to be as
Compose-like as possible, but I'm not going to expend any more effort with these stone tools than
is necessary to take the next step on my path to nirvana.

## Roadmap

This is a somewhat high level TODO list for C0. Normally I keep this sort of thing in an
uncommitted file in the top-level directory of a project, but this time I'll share with the class.

### Typing

- ~~Type `ImplDef` trees.~~
- ~~Introduce `RefTree` to bind particular symbol to reference in AST.~~
- ~~Include synthesized args for typeclass dictionaries in `Arrow` type. (As separate arglist?)~~
- Actually check that types match when typing trees.
- ~~Type pattern trees.~~
- Type comprehensions.

### Resolution

- Add an `ImplThis` tree node and use it when an impl binding requires passing the impl itself to
  the bound function.

### Lowering

- ~~Lower main AST to a simplified tree with no fancy features (pattern matching, monad
  comprehensions, blocks as expressions, etc.)~~
- ~~Make `lower` take a binding target to which to assign lowered expr instead of creating a
  fresh ident for every subexpr~~
- ~~Inject (or pass along) appropriate dictionaries in `Apply`.~~
- ~~Lower pattern trees.~~
- Lower comprehensions.

### Codegen

- ~~Walk the lowered tree and generate unoptimized JavaScript.~~
- ~~Wire up some kind of JavaScript FFI~~

### Modules

- Come up with syntax for modules and imports.
- Implement serialization for typed ASTs:
  - single module serialized into single blob.
  - contains id -> name table for all internally defined names + imported names.
  - tree itself uses ids everywhere, no names.
- Separate compilation will load stAST files for referenced modules.
- Code gen will convert group of stASTs into a blob of JS.

## Building and running

`C0` is written in [Scala] and built with [SBT] (lord forgive me). If you're not already a Scala
developer and know how to use SBT, you should probably just go back to whatever you were doing and
pretend you never read this document. However, if you love to suffer, then go ahead and install SBT
and hope that nothing explodes along the way.

The only interesting thing you can do right now is compile the code and run the tests:

```
sbt test
```

That should spew out a bunch of `C0` code which was pretty printed after being parsed along with a
bunch of (less pretty) printed typed ASTs. Thrilling!

[Scala]: http://www.scala-lang.org/
[SBT]: https://www.scala-sbt.org/
