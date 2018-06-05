# Compose Compiler / Editor Mark Zero Prime

This is an abandonded attempt at a bootstrap compiler and editor for Compose: `CE0'`, written in
PureScript.

## Initial motivation

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

## Reasons for abandonment

1. Documentation is lacking for the myriad standard libraries in the PureScript ecosystem. There is
   some nice tutorial style documentation (in the form of the PureScript book), but when it comes
   to actually writing your own code, you discover that most of the types and functions provided by
   the standard library are minimally documented. Anyone who is not already intimately familiar
   with coding in a pure functional style and these particular APIs (presumably most of which have
   been inherited from Haskell) is likely to struggle with the terse API documentation. If you're a
   life-long Haskell programmer, this is probably all great, but if you're learning how to use all
   of this pure functional machinery for the first time, it is quite frustrating.

   For example: `runState` is documented thusly: "Run a computation in the `State` monad". And
   indeed the entire `Control.Monad.State` module is documented as: "This module defines the
   `State` monad." In the case of the state monad, there are lots of tutorials on the interwebs
   that one can dig up (though most are written for Haskell, so one has to figure out the subtle
   translations needed to convert these to PureScript). But for less basic stuff, and for
   PureScript specific stuff, you're largely on your own. Not sure whether you need to use
   `liftAff` or `liftEffect` or quite how to interweave the half dozen monads that arise when doing
   anything non-trivial? You'll just have to try to puzzle that out from the regurgitated internals
   of the type checker when you get a unification failure between `t0 t1` and `Function (t2 ->
   Query t3)`.

2. Halogen builds up a very sophisticated typed representation of the DOM and complex machinery for
   manipulating it, and these sprawling hundreds of functions and data types are nearly entirely
   undocumented. Again there's a nice little tutorial introduction, which walks you through
   building something very basic, but once you start to use the API in anger, you quickly discover
   that you need to understand how it works at a much deeper level (which is fair). But your only
   recourse is reading thousands of lines of dense, often point-free, highly abstract code. Here's a
   benign example:

   ```purescript
   -- | Constructs a text node `HTML` value.
   text :: forall p i. String -> HTML p i
   text = HTML <<< VDom.Text
   ```

   What's `p`? What's `i`? What are these HTML values? Well, let's take a look at the type
   signature for `HTML`:

   ```purescript
   newtype HTML p i = HTML (VDom.VDom (Array (Prop (InputF Unit i))) p)
   ```

   Let's just say it's not immediately obvious what's going on there. There's no overview of how
   the type and computational machinery works, and there's a _lot_ of machinery in there to bridge
   the gap between the impure, non-functional DOM APIs and the pure functional APIs provided by
   Halogen.

3. For better or worse given the shitty state of software development in general, one frequently
   figures out how to solve a problem by finding an example where someone has solved a similar
   problem and (ideally) deciphering how they solved it, then applying what you learned to solve
   your own problem. PureScript is not (yet) a popular language, so there aren't a lot of code
   examples floating around on the Internet, which makes this approach difficult. But it is greatly
   exacerbated by the fact that PureScript has been changing rapidly, so when you do find an
   example of how to do something there is a high probability that it is wrong in half a dozen ways
   because things have changed since it was published.

   This is hard to avoid for a new language because naturally it will have few users and naturally
   it will evolve rapidly as the early adopters figure out new and better ways to use the
   mechanisms offered by the language. I had the "good fortune" of starting to use PureScript just
   a couple of weeks before the release of PureScript 0.12, where major changes were made to how
   effects are modeled, which necessitated the coordinated release of a huge swathe of the
   PureScript ecosystem. That was a painful process to stumble through.

4. I made the mistake of switching from using Bower for dependencies to `psc-package`, partly
   "inspired" by the disaster I created for myself by accidentally upgrading `psc` to 0.12 right in
   the middle of the major effort to release updated versions of all the 0.12-compatible libraries.
   `psc-package` is not yet a mature package management solution and should probably only be used
   for either trivial projects where it works out of the box or by people who are actually working
   on `psc-package`.

   I nearly immediately needed more packages than were included in the stock package set, so I had
   to learn what package sets were, figure out how to fork the default package set repo and create
   my own custom package set, figure out how to add packages to package sets, publish my package
   set, then figure out how to use a custom package set in my project. That was just to get things
   working. Now when I need to add a package, I have to go to my custom packge set repository, add
   the package there (from Bower), manually resolve and add any of its dependencies (because
   `psc-package` leaves that task to you, the package set maintainer), then push an update to my
   package set. Then, because I was not able to find any mechanism to tell `psc-package` "hey my
   package set is updated", I have to go into the internal directories maintained by `psc-package`
   and manually `git pull` to obtain my updated package set. Then if the updated package set
   updated the versions of any of the packages, I have to manually remove and reinstall those so
   they will be updated (I could not find any way to tell `psc-package` to upgrade packages that
   were already installed). Then finally I could use `psc-package` to install my new package.

5. The language server support is not very robust (also true of `psc-ide` itself as well, given my
   limited attempts to use that in conjunction with Emacs). After a few minutes of normal editing
   (which naturally results in periods of having broken / ill-typed code), the compiler process
   providing the language server info would stop responding to changes or queries. Since I didn't
   want to restart my editor every few minutes, I ended up mostly relying on the output of `pulp
   --watch build`.

   This meant going through the fun manual process of visually parsing the source file error
   position from the very verbose error message and then manually navigating to that precise
   offset: go to the right file, go to the right line, go to the right character offset. And
   character offset is important because in dense pure functional code, whether the error is at
   offset 23 or 67 makes a huge difference where you are in the AST. I should have probably written
   an editor extension to parse the compiler output and extract the error messages so that I could
   at least navigate immediately to the error position. But I hoped (irrationally) mind that maybe
   somehow the language server stuff would start working more reliably (that was also part of my
   motivation to upgrade to 0.12) and I could get back out of the stone age of looking at the
   output of a batch compiler.

6. Even when the language server was working, it rarely provided me with the information I wanted.
   Since pure functional code is dense and often has complex types, I was constantly trying to
   figure out the type of some particular sub-expression, but there's no way to do that (that I
   could determine). For example, in code like: `(HE.input UpdateText)` I can see that `HE.input`
   has type `input :: ∀ f a. (a → Action f) → a → Maybe (f Unit)` and I can see that `UpdateText`
   has type `UpdateText :: ∀ a. String → a → Query a`, but I'd sure like to see the type of
   `(HE.input UpdateText)`. You might think that I could just rewrite it as `let foo = (HE.input
   UpdateText) in foo` and then look at the type of `foo`, but that leads me to the next problem.

   Many of the named expressions had no type information: function parameters, let bound names,
   binders in pattern match expressions, lambda arguments, etc. So if I extracted an expression of
   interest into a let binding, I could not find out the type of the bound name.

   In some cases, the type information reported would even be wildly incorrect. For example in:

   ```
   appendBareType kpath ktpe = case ktpe of
     Const const -> do
       appendSpan $ M.typeSpan (todoEditor kpath) (show const)
   ```

   Hovering over `const` in `Const const` claims that the type of that term is the `const` standard
   library function. Not true!

All of these challenges are acutely frustrating to me because they are exactly the sort of problems
that I have with the status quo in software development, and they are exactly the problems that I
am working to remedy by creating Compose. But even knowing that I am using these problematic tools
to build better tools, I can only tolerate so much friction. Pure functional languages, in my
opinion, have an especially great need for robust, informative tooling, because pure functional
code is so dense and carries such complex types.

Anyhow, I don't write this as a condemnation of PureScript. I think it's a great language. The
reason I tried it is because the language itself is similar to my goals with Compose. I simply felt
that it would be worthwhile to enumerate the difficulties I encountered, before I went off to beat
my head against some other language ecosystem for a while.

[C0]: ../c0
[PureScript]: http://www.purescript.org/
