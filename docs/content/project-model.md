---
title: "Project Model"
---

Main concepts:

* A _workspace_ contains projects.
* A _project_ contains components.
* A _component_ is either an _app_ or a _lib_ and contains modules.
* A _module_ contains defs.

Projects in a workspace are either explicit (i.e. you added the project to the workspace and you
are working on it) or implicit (the project is a dependency of an explicit project or another
implicit project).

If an explicit project depends on another explicit project, the (bleeding edge) version in the
workspace will be used instead of the expressed version. This allows one to easily work on a
dependent and dependee together.

Projects are how code is published. A project is published at a given DVCS URL and other projects
depend on it via that DVCS URL. Projects (and components, and modules) are assigned UUIDs which are
how dependencies are tracked and resolved initially, but if a project moves from being published at
DVCS URL A to DVCS URL B, we will either need some sort of forwarding mechanism and/or dependent
projects will have to be manually updated to reflect the URL change (as there is no centralized
directory of projects).

Project components are either libraries (bundles of reusable code) or applications (code that
generates an 'executable' of some form; this may be a blob of HTML+JavaScript+CSS, or a jar file,
or an ELF executable, etc.). Components express dependencies on other components (which means a
dependency is DVCS URL + project UUID + component UUID).

When a project is added to a workspace, the dependencies of all of its components are resolved into
that workspace as well, potentially adding one or more implicit projects to the workspace.

When a component (A) depends on another component (B), defs in modules in A can reference (depend
on) defs in modules in B. Cross-module references are tracked via module UUID + def id.

Presently my intention is for only a single version of a project (and hence component or module) to
be in a given workspace. So all code that depends on a particular project must be compatible with
the single version made available by the workspace. If this proves untenable, then I'll reorganize
things to allow modules to depend on specific versions of other modules, but that seems like a can
of worms that is best avoided. It opens up all sorts of UI/comprehension problems. Function A
depends on version 1 of function B but function C depends on version 2 of function B, etc.
