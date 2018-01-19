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

## Building and running

`C0` is written in [Scala] and built with [SBT] (lord forgive me). If you're not already a Scala
developer and know how to use SBT, you should probably just go back to whatever you were doing and
pretend you never read this document. However, if you love to suffer, then go ahead and install SBT
and hope that nothing explodes along the way.

The only interesting thing you can do right now is compile the code and run the parser tests:

```
sbt test
```

That should spew out a bunch of `C0` code which was pretty printed after being parsed. Thrilling!

[Scala]: http://www.scala-lang.org/
[SBT]: https://www.scala-sbt.org/
