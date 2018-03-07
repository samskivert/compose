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

### Type checking

- Actually check that types match, instead of just typing trees.
- Type (and check) `ImplDef` trees.
- Introduce RefTree to bind particular symbol to reference in AST.
  - This will be needed so that we can resolve typeclass methods during typing and then save the
    result for later interested parties (like code generation).
- Include synthesized args for typeclass dictionaries in Arrow type. (As separate arglist?)
- Inject (or pass along) appropriate dictionaries in FunApply.

### Code generation

- Walk the tree and generate unoptimized JavaScript.
  - Not sure if we even care to create an intermediate representation for C0 because its only
    purpose in life is to enable E0 to be written in something approximating C1, so it doesn't
    need to generate especially performant code.

### Pattern matcher

- Type pattern trees.
- Do codegen for pattern trees.
  - Consider whether we want to do any lowering or optimization prior to codegen. Probably not
    (cf. note in codegen about C0's short lifespan).

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
