---
title: FAQ
---

Wherein questions which have in fact not been asked with any degree of frequency are misclassified
as frequently asked, and then answered.

## What's the programming model?

Compose is at its core a strict, pure functional programming language, with ADTs and pattern
matching. It is not total. We like big bottoms and we can not lie. It will have a carefully
specified (stolen?) semantics. It is in the spirit of [SML]/[OCaml], but with an eye toward more
modern syntax, like [Reason].

Compose does not handle state with monads, it uses an isolated, mutable, reactive state repository
(think more 'fast database inside my program' than 'mutable heap') and a separate class of
computations that transform that state. _Functions_ are pure computations that take inputs and
compute outputs, just like God and Leonhard Euler intended. _Reactions_ (name may change) are
computations that are triggered by a change in reactive state, which may make use of any number of
functions to compute new values, which are then used to update reactive state.

This description is sketchy because I have not ironed out all the details, but the iron is coming. I
have more detailed early notes on reactive state in a blog post on [reactive state programming].

## Does it orient objects?

Compose is not an object oriented language. There are certain things about OO that are nice, and
Compose will seek to include them (first argument dot function name style calls, for example,
because they're so tab completable). William R. Cook wrote a great paper [On Understanding Data
Abstraction, Revisited] which breaks down OOP into its many components and compares and contrasts
them with alternative approaches.

Compose will likely achieve ad-hoc polymorphism via a mechanism like Haskell's type classes. Some
languages (like Scala) use classes and objects to obtain modularity and encapsulation, Compose will
use modules for that. Classes are also used to bundle up a package of state and functions that
operate on that state. Compose will support this via something more like what I called a "system"
in my post on RSP (linked above). Systems have the benefit of making lifecycle a first class
concept, which is something classes and objects often leave as an exercise to the reader.

Systems also don't come with the poison pill of method overriding. Overriding methods as extension
points and as a way to customize code is a failed experiment. We can see that written on the walls
with "prefer composition over inheritance". Aspect oriented programming illustrates other ways that
it has failed. Let's try some new ideas.

The one thing Cook nails down as fundamentally OO is dynamic disaptch through the `this` pointer.
That is, you don't know what code is going to run until you follow the vtable pointer and find out
where you end up. Megamorphic call sites anyone? This is something Compute is going to try hard to
avoid. I think the other benefits of the "OO package" can be achieved via different mechanisms, and
this particular bit of dynamism/polymorphism can be left by the wayside.

## Will it have null?

Jesus no! Why would you even ask that question? Are you some kind of anarchist?

## What will it run on?

Initially Compose will compile to JavaScript (so that I can run in the web browser), and to Java
bytecode (so that I can run on a mature, high performance VM). But my goal is to eventually
generate machine code with an LLVM backend or something like it. I'll probably also support
WebAssembly, why not? That is not to say that the goal is machine code generation and the other
backends will go away once that is available. My goal is to support different backends for
different use cases.

If you're building an iOS app, you have certain needs, whereas if you're building a website or
microservice your needs are very different, different still if you're building a video game (which
is my background). It's also important to me to be able to leverage the substantial body of
debugging, performance anaylsis and performance tuning tools that have been created for mature
platforms like the JVM. I'm going to reinvent a lot of wheels with Compose, but I'd like to
leveragae existing wheels where I can, at least until I get around to reinventing them.

I am confident that I can achieve tolerable semantics with LLVM, and I think the JVM is even on a
trajectory that will enable the things I want to do without forcing things too much to be modeled
as objects behind the scenes. JavaScript will always be a problem child, but the main goal there is
to be able to have interactive playgrounds where you can build and play with programs of modest
size, so I'm not as worried about performance. I will make every performance sacrifice necessary to
ensure that JavaScript's "semantics" do not leak through, and that Compose on a JS VM functions
correctly and without compromise.

## Why is this documentation so tongue in cheek?

Programming computers is a defiant raising of the middle finger at the inexorable destructive
forces of complexity. In the end complexity will win and all of our cute little attempts to tame it
will blow away like so much dust in the wind. The universe will laugh in its cryptic way and things
will proceed stonily toward eventual heat death. In the face of that kind of existential terror,
one needs a little humor to keep the wheels moving.

## When will it be done?

Never! But if you mean when will there be something I can play with, probably not for at least a
year. I'm budgeting a decade, but that's for the full monty. I should hope that I can get a
self-hosting compiler up and running in a year or so, along side a basic smart editor. It took
three months to get my (comparatively modest) [IDE project] to the point where I could work on the
IDE in the IDE. This will take longer. 2x, 4x, who knows? Plus I can't even start in earnest until
September.

If you're reading this at all, you're someone I'm just hoping to engage in discussion to help
provide good ideas and bad advice as I embark upon this (new) quixotic journey.

[reactive state programming]: http://samskivert.com/blog/2013/11/thinking-aloud-rsp/
[SML]: https://en.wikipedia.org/wiki/Standard_ML
[OCaml]: https://en.wikipedia.org/wiki/OCaml
[Reason]: https://facebook.github.io/reason/
[On Understanding Data Abstraction, Revisited]: http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf
[IDE project]: https://github.com/scaled/scaled
