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

> Note: this list is in-progress and incomplete.

## Programming model

### Functional core

* Strict functional core language. Possibly with constrained local mutation (for those times when a
  while loop is clearer than tail recursion).

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

* A Compose project is a database of serialized ASTs, not a collection of text files. Your desired
  meaning is never squeezed through the paltry tube of ASCII text and then reconstructed by the
  compiler, usually poorly due to the competing tensions of clarity, aesthetics, verbosity and
  grammar complexity. All of these tensions melt away when projectionally viewing or editing a rich
  AST that explicitly records every symbol and every type as resolved at the time the AST was
  created.

* Editing will "look and feel" as much like editing text as possible, but constrained in just the
  right ways such that syntax errors remain impossible. Edits operate on the AST and while they may
  leave holes in the AST, they always preserve a syntactically valid tree. Making this ergonomic is
  thus far the hardest problem I am tackling, but I remain optimistic that it is possible.

* Names are resolved and the AST is type checked as you create it. Type inference is a conversation
  between you and the compiler, the results of which are recorded in the AST (and which may or may
  not be visible depending on your preference). You are never surprised because a change in one
  part of your code resulted in a far reaching reinterpretation of the meaning of other parts of
  your code. In some cases, it may make sense to inform you that such reinterpretations are
  possible and give you the option to perform them, but this will be a controlled, opt-in process,
  not a ticking time bomb.

* The grid of characters that you see in the editor is a visualization of the AST, not the "ground
  truth" of your program. This means each individual that reads or works with code can make their
  own choices about how much detailed type information they want to see and when. A novice
  programmer may wish to see type annotations on everything, and lean heavily on the ability of the
  editor to show them the types of intermediate expressions. An experienced developer may opt to
  show no types at all, and only periodically double check with the editor that their mental "type
  machinery" came up with the same type assignments as the language. This need not be a "one size
  fits none" universal mandate due to the historical choice of representing the AST as a "human
  readable" sequence of ASCII characters.

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

## Reading and Editing

* Code is a hypertext and there are many ways to organize and view it. By eliminating "text in
  source files" as an implementation artifact, I plan to shift the focus away from this incidental,
  implicit organization (the order in which definitions appear in a file, the name of the file, the
  names of directories that contain the source files) and toward explicit semantic organization.
  This can be expressed by the original author (grouping definitions into modules), or created
  dynamically to solve a problem at hand (show all definitions used by a particular function, or
  show all definitions that call a particular function). There are many useful ways to dynamically
  organize code. Other examples: showing only the exported definitions of a module, or group of
  modules, showing code not reached by any test code, showing definitions alphabetically or in
  'curated' order.

* Because Compose is oriented around modules and definitions, rather than files, the editing
  experience will be more like [Code Bubbles] than [Visual Studio]. I'm currently leaning toward
  something slightly less free-form than Code Bubbles, but you'll still have a lot of flexibility
  in arranging your workspace based on your problem at hand and not based on the incidental
  structure of streams of characters, text files and directories.

* Before going completely crazy and deciding to make my own language, I was very interested in
  visualizing large projects. I have read a great deal of interesting work on architecture
  extraction (TODO: link) and on language features that facilitate automated generation of
  visualizations of project structure (like ownership type systems (TODO: link)). I plan to
  experiment with "overview" visualizations that allow someone to get a feel for the structure and
  organization of a large code base, and then use that overview as a starting point to explore
  further. These explorations will be facilitated by related features, like the earlier described
  mechanisms for showing "slices" of the code base that make use of a particular type or call or
  are called by a particular function, etc.

## Documentation

* Documentation is a first-class citizen. It is structured (has its own simple AST), checked (can
  contain references to definitions which are "live"), and can contain embedded "live" code
  examples (real checked and compiled code running on real data).

* Detailed documentation and executable code examples can appear "next to" the code in question.
  Because we're not limited to static text files, a library author can simply denote code as being
  example code and the editor/environment can surface appropriate example code for any function or
  data structure on demand.

* A project should be able to include _all_ of its documentation. Documentation should not be split
  across an overview/examples website, generated API documentation and the code itself. Overview
  documentation, tutorials, examples, everything should be able to live inside the project and
  benefit from the structure, checking and liveness that one has when reading code in an IDE.

## Projects, building

* Many languages prescribe some aspects of code organization, but leave other aspects undefined.
  The community has to come up with ad-hoc tooling to finish the job: IDEs, build systems, unit and
  integration test frameworks, and so forth. These often each invent their own incompatible notion
  of how to express project components, artifacts and dependencies, where intermediate build
  results are stored, etc. At best the tools manage to interoperate via an impoverished, least
  common denominator project model and at worst the developer has to write and maintain a bunch of
  manual glue to get these tools to interoperate, or rely on a diaspora of often poorly maintained
  plugins.

  Compose will define a complete model that spans entire projects down to individual expressions,
  and includes additional concepts needed by the actual development process. I have a separate page
  with details on the [project model](../project-model/).

* One tension that often comes up when reusing code maintained by a third party is how to resolve
  situations where the code doesn't quite do what you want. In very "permissive" langauges like
  JavaScript, developers have a lot of leeway to patch things at runtime, an expedience that
  usually results in disaster. In OO languages, developers can sometimes override methods and tweak
  behavior to accomplish their needs, or in extreme cases use reflection to access and change class
  internals. Or a library can be copied wholesale into your project and tweaked, at which point
  you've taken on the burden of manually merging future updates or abandoned the possiblity of
  benefiting from them. These "solutions" range from disastrous to merely highly undesirable.

  Rather than attempt to design mechanisms into the language to facilitate "hooks" and
  customization, I plan to support the maintenance of local patches against library dependencies.
  At any point, you can simply edit the code of a project dependency, and the system will maintain
  those edits as a diff against the library. When you upgrade the dependency, the patches will be
  reapplied and if they can be applied cleanly, you have nothing else to do. If not, you have to
  adapt your patches to the updated library. This is not a "zero effort" solution, but of course no
  such solution could possibly exist. This solution keeps you honest, in my opinion, about what you
  are doing: changing someone else's code without their knowledge. That comes with risks, so best
  not to hide those risks, but rather to help you manage them.

  This also enables you to make the simplest possible change to the target code: no need to
  override a method and bend over backward to fit your change into places where extension points
  intentionally or accidentally exist. If you need to change a `3` to `4` then you just make that
  change. If you need to add an additional field to a record, no need to create a subclass and try
  to sneak that through the rest of the code undetected, just add the field to the record that
  needs it.

## Testing

* Unit testing will be "built-in" and will benefit from the ability to easily expose dynamic views
  of the codebase. Because the compiler infrastructure will be readily available to the unit
  testing infrastructure, automatically running "affected" unit tests on changes should be
  straightforward. Test feedback should be as automatic and "ambient" as type feedback is in a good
  IDE.

* Integration testing will likely manifest as additional application/executable project components.
  An integration test is a built artifact, just like an application. It may prove more challenging
  to manage the actual execution of said integration tests due to the necessity of integrating with
  production or staging environments and the myriad moving parts that come with actual deployment.
  It's something that will have to evolve as we obtain experience with building real apps.

## Debugging

* Streamlined mechanism for tracing expressions and function calls: can take the form of logging,
  or a 'watched variable' like a debugger, or a visual display like a graph of a numeric value. The
  potential exists for fancier visualizations, but I'd like to start with the low hanging fruit.

* The reactive state model will support inspecting/visualizing that state during program execution
  (and after, for state that can persist across executions). But I would like to also visualize
  state dependencies, and changes as they propagate through the data flow graph.

## Version control

* Not using text files to store source code will present challenges to making use of existing
  version control systems. This is an anticipated pain that I think is well worth the many other
  benefits derived from freeing ourselves from the tyranny of text. Through a combination of IDE
  integration with version control systems, a VCS friendly serialized AST representation, and
  custom diff tools, I think these hurdles can be overcome. If artists can diff Photoshop files,
  then surely we can manage to diff serialized ASTs.

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
