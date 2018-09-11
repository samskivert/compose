---
title: "Goals"
---

> It must be borne in mind that the tragedy of life doesn’t lie in not reaching your goal. The
> tragedy lies in having no goals to reach.” — Benjamin E. Mays

I have many goals for Compose, some radical, some mundane. Enumerating them here serves to convey
my aims until that job can be done by a working prototype. Communicating the goals also helps me to
clarify them to myself. Hello self!

Some of these goals are nebulous and will require research to further clarify and achieve; some are
straightforward and "merely" require a lot of engineering work. In the fullness of time I hope to
address them all, as my overarching goal is for Compose to be used (at least by me) to build real
software that improves the world, not just a research prototype.

## Programming model

### Functional core

* Strict functional core language. Possibly with local impurity (for those times when a while loop
  is just much clearer than tail recursion).

* Sums and products (algebraic data types) for composing scalar data into structures.

* Type classes for ad-hoc polymorphism, including a standard library of the [usual suspects]. See
  the [FAQ](../faq/) for some ranting on why no OOP. I have vague goals to incorporate aspects of
  ML-style modules, but they are insufficiently baked and I cannot yet describe them.

* Effects: possibly modeled through [Frank]-like effects system, possibly modeled through
  "capability arguments", possibly something else. I'm really hoping _not_ to model them via
  [MTL]-style type class boilerplate.

* "Rule-abiding" numeric types (arbitrary size naturals and integers, and arbitrary precision
  decimals) for when semantics are more important than performance. I plan to be very explicit
  about the types which prioritize performance over semantics. For example, there will be no `int`
  type, there will be a '32-bit signed twos-complement integer' type, which wears its particular
  semantics on its sleeve.

* I will probably also include a "reasonably performant numeric type that has neither precise
  semantics nor precise performance characteristics, but will probably do what you want as long as
  your numbers aren't too big" abstraction to paper over the annoying variety in the backends I
  hope to eventually support (some subset of JavaScript, WebAssembly, JVM, CLR, LLVM).

* `String` will be an abstraction, rather than a concrete data type, so that the privileged
  representation for a particular backend platform can be used by default.

### Reactive / dataflow

* The standard library, the environment, and potentially the language, will provide tools for
  "reactive" programming.

* My design is somewhere in between the stateful one-way dataflow (popularized by [React] and
  [Elm]), and the "everything is a fold over streams" higher-order FRP more common in pure
  functional languages (like the [Reflex] library). I described my ideas as [RSP] a few years ago,
  but things will likely evolve from there when actually implemented.

### Datalog

* I'm intrigued by the pervasive decoupling enabled by datalog-style programming (see this
  exposition of its use in [RealTalk]), but I need to experiment more with it before understanding
  how much it would benefit from being deeply integrated into the language.

## Syntax and type checking

* Serialized ASTs, not text.

* Edit the AST not a grid of characters.
  * Syntax errors become impossible.

* The AST is type checked as you create it. Type inference is a conversation between you and the
  compiler, the results of which are recorded as type annotations. It's not a fragile process
  repeated every time a program is compiled.

* Visualize the AST, rather than a grid of characters.
  * Type annotations exist but need not always be shown. The developer can decide what to show by
    default, what to show interactively, etc.

## Refactoring

* Names are merely annotations for humans. Changing them does not change the structure of the code
  and requires no changes to any code using the changed definition.

* AST edits capture more semantic information than textual edits. That information can be used to
  construct patches which are automatically or interactively applied to callers of the changed
  code.
  * For example, adding an additional argument to a function could also capture a default. When a
    user of the changed library updates their dependency, it could trigger an automatic or
    interactive patching process which applies that default argument to all calls, allowing the
    user to inspect each call site and accept the default or make an alternative choice.
  * As another example, factoring some functions and data structures into a sub-module should not
    require manual repair by calling code. The change in provenance can be recorded and applied
    automatically or interactively when calling code upgrades to the new version of a library.

## Code organization

* Many languages prescribe some aspects of code organization, but leave other aspects undefined.
  Build systems and the community have to come up with ad-hoc tooling to finish the job. Compose
  will define a complete model that spans entire projects down to individual expressions, and
  includes additional concepts needed by the actual development process. I have a separate page
  with details on the [project model](../project-model/).

* Code is a hypertext and there are many ways to organize and view it, explicitly and implicitly.
  By eliminating "text in source files" as an implementation artifact, I aim to shift the focus
  away from the incidental organization it implies (the order in which definitions appear in a
  file, the name of the file, the names of directories that contain the source files) and toward
  explicit semantic organization. This can be expressed by the original author (grouping
  definitions into modules, for example), or created dynamically to solve a problem at hand (show
  all definitions used by a particular function, or show all definitions that call a particular
  function), or because there are simply multiple useful ways to organize code (show only the
  exported definitions of a module, or group of modules, show code not reached by any test code,
  show definitions alphabetically or in 'curated' order).

## Documentation

* Documentation is a first-class citizen. It is structured (has its own simple AST), checked (can
  contain references to definitions which are "live"), and can contain embedded "live" code
  examples (real checked and compiled code running on real data).

* Detailed documentation and executable code examples can appear "next to" the code in question.
  Because we're not limited to static text files, a library author can simply denote code as being
  example code and the editor/environment can surface appropriate example code on demand for any
  function or data structure on demand.

* A project should be able to include _all_ of its documentation. Overview documentation,
  tutorials, examples, everything should be able to live inside the project and benefit from the
  structure, checking and liveness that "API" documentation benefits from.

## Testing

* Unit testing will be "built-in" and will benefit from the ability to easily expose dynamic views
  of the codebase, and because the compiler infrastructure will be readily available to the unit
  testing infrastructure, automatically running "affected" unit tests on changes should be
  straightforward.

* Integration testing will likely manifest as additional application/executable project components.
  An integration test is a built artifact, just like an application. It may prove more challenging
  to manage the actual execution of said integration tests due to the necessity of integrating with
  production or staging environments and the myriad moving parts that come with actual deployment.
  It's something that will have to evolve as we obtain experience with building real apps.

## Building

* TODO: Patching of library dependencies.

* TODO: Build specification & extension mechanism built-in.

## Debugging

* Streamlined mechanism for tracing expressions and function calls: can take the form of logging,
  or a 'watched variable' like a debugger, or a visual display like a graph of a numeric value. The
  potential exists for fancier visualizations, but I'd like to start with the low hanging fruit.

* The reactive state model will support inspecting/visualizing that state during program execution
  (and after, for state that can persist across executions). But I would like to also visualize
  state dependencies, and changes as they propagate through the data flow graph.

* ... TODO ...

## Editing and Tooling

* Because Compose is oriented around modules and definitions, rather than files, the editing
  experience will be more like [Code Bubbles] than [Visual Studio]. I'm currently leaning toward
  something slightly less free-form than Code Bubbles, but you'll still have a lot of flexibility
  in arranging your workspace based on your problem at hand and not based on the incidental
  structure of characters, text files and directories.

* ... TODO ...

## Version control

* ... TODO ...

## Code visualization

* ... TODO ...

## Learning

* Use mathematics terminology and symbols where appropriate, to leverage students (and adults)
  prior investment in that "language".

* The editing environment can be tailored to show more or less contextual help based on the skill
  level of its user. This may manifest as preference toggles for myriad help mechanisms (e.g. show
  types on locals, show key bindings, verbose type error hints), also controllable via
  coarse-grained "beginner, intermediate, advanced" presets, or perhaps something fancier.

* TODO: learnable key bindings

### Microworlds

* To enable a focus on specific concepts, I aim to create [microworlds] which combine a restricted
  set of functional building blocks with a high-level simulation or framework. The simulation will
  be embedded in the editor so that the entire experience takes place in the single unified
  environment.

* For example: a 2D drawing microworld provides functions that describe simple geometric shapes
  (possibly evolving over time) for a built-in framework that draws and animates the shapes.
  Similar in the spirit to the [ShapeCreator] created for the [Elm Graphics] program at McMaster
  University. This focuses on the concepts of function composition (chaining functions to create a
  particular shape), function reuse (encapsulating a shape into a function and reusing it in larger
  shapes), looping/repetition (repeatedly calling a function with slightly different parameters).

* Another example: a simulated robot moving around an environment (ala [Robot Turtles] or
  [LightBot]) which the student controls by creating a list of instructions. "Instruction"
  functions compute a new state for the robot based on its current state and "sensor" data from the
  environment. This can focus on the concept of sequenced computations (executing one instruction
  after another), and on the concept of an abstract state which represents an evolving entity.

* More advanced microworlds can focus on more abstract scenarios like computing and graphing
  arithmetic expressions, transforming and processing data, creating simple user interfaces. They
  would all leverage the ability to embed the target application in the editing environment, and
  the ability to restrict the editing environment to just a [Scratch]-like palette of relevant
  (functional and structural) building blocks.

* As the students projects scale up in complexity, they are able to leverage the organizational
  features of Compose and its editing environment just as a professional programmer would. They can
  define their own data types, group code and data into modules, document and test their code. Many
  of these capabilities are lacking in traditional pedagogical programming environments, which can
  lead to frustration and miss an opportunity to educate the student on good practices for dealing
  with the inevitable complexity that comes from larger programs.

[RealTalk]: https://rsnous.com/posts/notes-from-dynamicland-geokit/#background-on-realtalk
[React]: https://reactjs.org/
[Elm]: http://elm-lang.org/
[Reflex]: https://github.com/reflex-frp/reflex
[RSP]: http://samskivert.com/blog/2013/11/thinking-aloud-rsp/
[Frank]: https://arxiv.org/abs/1611.09259
[MTL]: https://github.com/haskell/mtl
[usual suspects]: https://wiki.haskell.org/Typeclassopedia
[Code Bubbles]: http://cs.brown.edu/~spr/codebubbles/
[Visual Studio]: https://code.visualstudio.com/
[microworlds]: http://edutechwiki.unige.ch/en/Microworld
[ShapeCreator]: http://outreach.mcmaster.ca/tools/ShapeCreator.html
[Elm Graphics]: http://outreach.mcmaster.ca/what-we-do.html
[Robot Turtles]: http://www.robotturtles.com/
[LightBot]: http://lightbot.com/
[Scratch]: https://scratch.mit.edu/
