---
title: Manifesto
---

When possible, I like to start my projects with a good manifesto. Decry the moral bankruptcy of my
peers, chart out a course to utopia, that sort of thing. Here are the things that Compose aims to
do that I feel buck the status quo to varying degrees.

## No files, no ASCII

This has two major components:

1. Compose programs will not be stored as a sequence of UTF-8 characters that are parsed into an
   AST. Compose programs will be stored as a serialized AST. That serialization will be moderately
   human readable, but only incidentally.

   "What?!" you say, "but what about my command line tools? What about viewing diffs on Github, in
   email, in my favorite code review tool, in my hex editor while viewing raw disk blocks?" Those
   tools will have to change (maybe not the hex editor). We'll make it as easy as we can to squeeze
   a pretty printer into the toolchain, but ASCII's time is over. We must stop the madness.

2. Compose will not use the file as its primary compilation unit. We don't even have files, so it
   would clearly be a weird thing to do. Files as compilation units is a useless legacy from the
   days of the PDP-11 when that was all we could fit into memory. The natural compilation unit is
   the module. A module defines the connection point between separately compiled pieces of code. A
   Compose program is built out of modules, and a module is built out of definitions. There's
   nothing in between. (There is a name for a group of modules, it's a library, but we'll get to
   that later.)

## You create and edit the AST

This is sort of a corollary to the "No files, no ASCII" point. Compose programs are created in a
smart editor. You don't type in or change characters to create a program. You edit the AST. Just
like you don't edit the raw curve definitions in an SVG or manually execute Bresenham's line
algorithm to draw a line in a PNG, you don't change individual characters to edit a Compose
program. There is no such thing as a syntax error.

"What?!" you say, "but I'm a cave programmer. I cannot function without the soothing feels of vi or
emacs responding to my keyboard caresses." I feel you my friend. I too am a cave programmer. In
fact, I love the cave so much that I built my [own cave](https://github.com/scaled/scaled) and I
pimped it. I will make accommodations to the cave programmer, as I must, but Compose takes the long
view. We will soon be dead. And the next generation of programmers will look upon our tools and
techniques and say "Weird. Why would you do that?"

## Module oriented

Other useful things fall out of the non-use of files as compilation units and the orientation
toward modules as the fundamental unit of grouping code. You express your dependencies for a module
once (i.e. you import things once, not at the top of every goddamned file). You do any
renaming/aliasing of imported names once.

Visibility and accessibility (think private, protected, public) are simplified. Inside a module
everything is visible and accessible to everything else. There are no ABI concerns inside a module,
a module is compiled as a single unit. You don't need to hide things from yourself inside a module.
You think about one thing: what API does this module export for the rest of the world to use.
Syntactically, there's module internal, the default, and exported: `foo` and `export foo`. That's
it.

## Interactive compilation

The Compose compiler will be built first and foremost to deliver the best interactive code editing,
testing and execution experience possible. Only secondarily will it provide batch compilation. It
boggles my mind when new language after new language shows up to the party with a batch compiler
and zero thought given to interactive code editing. Do these people write computer software? Fast,
accurate, useful feedback to the programmer is *the most important thing!* Hello?

Anyway. The Compose compiler will run as a daemon, will load up, type check, and generate code for
all the code in one or more modules (or hey, maybe all that work will already be done because we
*don't use ASCII files to save our programs!),* and then it will wait for your commands. You will
deliver AST edits to it, or rather the editor will do so on your behalf, and the compiler will
propagate those changes through its many graphs and trees and update that which changes. It will
not whack itself over the head with a hammer, forgetting everything it has computed and then
recompute everything from scratch. Why would you do that?

My metric is not kLOC compiled per second. My metric is how many milliseconds it takes from
changing code and pressing the 'test' or 'run' button to seeing test results or executing software.

## Compiler is a library

Since I'm complaining about how everyone else builds compilers, here's another mistake Compose is
not going to make. The compiler will be built from the ground up as a collection of modules that
are designed to be used by third parties. Want to load a Compose serialized AST? No problem! Use
*the* AST loader. Not, "Oh, you probably have to use some half broken third party code because the
compiler is intertwingled with unwanted crap in sixty five ways, and wasn't designed so much as
regurgitated into files by a series of paper deadline addled grad students."

This will go all the way down. Need a typed AST, can do. Need to generate code, no problem! I also
have plans for metaprogramming capabilities that will undoubtedly be simpler than using the main
compiler machinery directly, but not to the unfathomably stark degree that you'll find in most
non-trivial languages.

## Editor and other tools are also libraries

Compose will come with a powerful editor that is designed to work harmoniously with the way Compose
programs are constructed. It will come with a build system and means to manage library (aka
package) dependencies for a project. These components will all be designed as a third party usable
modules, and the Compose tools will themselves be clients of these modules. Want to write a command
line tool that does custom crazy stuff: iterates over the library dependency graph, or the AST of
all code in a project, or over the build graph, you can! And you'll do it in Compose, not Perl or
Python or heaven forfend, Bash.

## Code organization support

Most languages give you 1960s state of the art tools for organizing your code: files and
directories. Thank goodness we nailed that one fifty years ago and have no need whatsoever to
improve upon it. Compose dispenses with files and directories and brings us at least a few decades
closer to the present.

Code is a hypertext. Code is a database. Compose and its editor, treat it as such. You can view and
browse your code in whatever way you find appropriate for your immediate circumstance: view a list
of modules with nested lists of all the definitions in those modules. Filter just the public
definitions for modules you're using, see the all definitions for modules you're writing. View all
the definitions (across modules) potentially touched by following the control flow of a given
function. View all the definitions that make use of a particular data structure.

The editor is designed to edit definitions, not files. We'll have some pretty pictures elsewhere,
but you can open definitions at the bottom of your current 'stack', or open a new stack in a
separate column, drag things around to arrange them in whatever way works best for your current
workflow. Have a stack of defs from the module you're working on, and a stack of defs from other
modules that you're referring to one column to the right. Have five columns and two dozen defs on
screen at once. Monitors are big.

## Documentation is a first class citizen

Most programming languages relegate documentation to raw or at best semi-structured comments. Then
they proceed to mostly ignore them, not even including them in their ASTs in many cases. Maybe they
give you some half-assed tools to 'extract' that documentation into HTML files, because *that's* a
great way to learn about someone else's software.

Guess what guys? Documentation is as important as code, maybe more! Documentation is not a
*comment*. A comment is something like 'Beware of bugs in the above code; I have only proved it
correct, not tried it.' Documentation is an explanation, a transfer of information from human to
human, a living evolving artifact that coexists with, and complements the code.

In Compose, documentation is a kind of code. You write it with a smart editor, it's checked by the
compiler. It may contain references to definitions; they're checked. It may contain executable code
examples, which are type checked and compiled and can be run interactively with the results
displayed in the documentation. It's in the AST! All the standard tools know about it, process it,
treat like the very important artifact that it is.

## Learnability is a primary goal

No sane person would try to create a new programming language. Fortunately, I don't count myself
among the ranks of the sane, so that's not a problem for me. But I also have a good lie that I can
tell myself to keep me toiling in obscurity day after day, year after year. Compose is a language
designed for teaching computer science, and as we know students don't get to choose the language,
the teacher does.

I aim to educate a new generation of software developers, untainted by the burden of history. And
not with a language that's obviously pedagogical, one that clearly must be discarded when moving on
to making "real" software. Compose is a language that scales down to learning the basics of
computation, and scales up to the challenges and horrors of modern software development.

Making real software is unavoidably complicated. You have to build on platforms that have been
around for decades and are warty and fragile. Compose will have mechanisms (like external reactive
state) for adapting and "cleaning up" these crufty platform APIs so that they fit more nicely into
its programming model. But it can't fix everything, it will simply strive to ease students into the
complex reality of real APIs and platforms.

I plan to build a series of successively more complex "playgrounds" in which to learn fundamental
concepts and to build more and more interesting programs. The curtain will be gradually pulled
back, bit by bit. By the time you graduate to a context where you have access to, say, the entire
Java SDK, or all iOS platform SDKs, you will have girded your loins with the creation of much
civilized software and you can wade into the mire with naught but a wistful sadness at how much
better it ought to be.

## Yeah, but does it use curly braces?

I have various other goals with the language and environment that are more specific to what is
traditionally considered the "language design". A lot of these are just stolen from existing
languages, so they're not bucking the status quo, just picking and choosing from it. One major
component is to incorporate ideas from [reactive state programming] that I described a while ago.
Other ideas have been described on my short-lived [bikeshed] blog. See the [FAQ](FAQ) for more
details on things like the syntax, programming model, implementation plans, etc.

[reactive state programming]: http://samskivert.com/blog/2013/11/thinking-aloud-rsp/
[bikeshed]: http://samskivert.com/bikeshed/
