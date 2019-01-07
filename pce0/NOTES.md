# Design notes

## Compiler & AST design

TBD

```purescript
data Tree =
  -- definition nodes
  Def name body
  Data name record+
  Record name field*
  Field name type
  -- expression nodes
  Lit value
  Ref name
  App fun arg
  Let def
  Abs arg body
  If test true false
  Cond (test body)+
  Case scrut (pat guard? body)+
  ... where, type, interface, impl, etc. ...
```

## Editor design

### Markup model

The AST is transformed into a markup model which is used to display the code in a familiar "syntax
highlighted, formatted text" format, but the text itself is not editable. Edits are made on the
underlying AST and those changes propagate back out to the visualization.

```purescript
data Defn = List Line
data Line = List Span
data Span = {
  text :: String
  editable :: Boolean
  styles :: List String
}
```

### Cursor and selection model

The markup model is to display the code, and a cursor and selection model are used to control
editing operations and visualize where they will take effect.

```purescript
-- A path through the AST from the root of the top-level definition to a leaf node
data Path = List Tree

-- Models the editing cursor: indicates whether we're currently editing an editable leaf node
-- (literal or name) and the path to that node. If we're not editing, the path identifies the
-- node where editing will start if a "begin editing" operation is performed.
data Cursor = {
  path :: Path
  editing :: Boolean
}

-- Models the editing selection: a range of nodes that are selected (and visually highlighted) so
-- that they may act as the target of an editing operation (like cut, extract into binding, etc.).
-- The cursor must always be inside the selection. When the cursor is moved, the selection is
-- reset such that it starts and ends at the cursor's node.
data Selection = {
  start :: Path
  end :: Path
}
```

### Operations

Editing operations are initiated via key bindings, and trigger either the insertion or removal of
one or more AST nodes, or "open" certain nodes (lit and ref) for free-form editing.

#### Create def

Creates a new top-level definition (trigger: Cmd-N?). A placeholder (hole) is created immediately
for the documentation and name of the def:

```
[Describe this definition...]
[name]
```

The cursor is initially in the `documentation` field, which is editable. The programmer can press
TAB to switch to the `name` field (which will be made editable). While editing the name, pressing
SPACE will create a hole for an argument and activate it (immediately move the cursor to it and
make it editable):

```
Reverses the supplied list.
reverse [arg] = [body]
```

Alternatively, the programmer can press `{` to create a hole for a type argument:

```
Reverses the supplied list.
reverse ∀[type arg] = [body]
```

Or perhaps just entering an upper case argument creates a type arg and lower case arguments are
term args.

While editing an argument, pressing `:` will create and activate a hole for the type of the
argument:

```
Reverses the supplied list.
reverse ∀A as :[type] = [body]
```

When entering the type, completion will be performed on known types. If the selected type takes
arguments, one or more holes will be created for those as well.

```
Reverses the supplied list.
reverse ∀A as :List [?] = [body]
```

If a name is provided for the type argument that does not match a known type, an option will be
provided to declare that name as a type argument, in which case it will be added to the function
signature.

While editing an argument type (or an argument of the type), pressing `:` again will create and
activate a hole for the return type of the function:

```
Reverses the supplied list.
reverse as :List A -> [type] = [body]
```

Pressing `=` at any point during signature definition (or TAB) will move to the hole for the body,
removing any trailing unused argument slot.

```
Reverses the supplied list.
reverse as :List A -> List A = [body]
```

```
Reverses the supplied list.
reverse as :List A -> List A = [body]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let [name] = [value] in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc [arg] = [value] in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as [arg] = [value] in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = [value] in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case [] of
    [match] -> [value]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    [match] -> [value]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> [value]
    [match] -> [value]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    [match] -> [value]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons [] []-> [value]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h [] -> [value]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> [value]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc [as] [acc]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc t [acc]
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc t h
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc t h :: []
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc t h :: acc
  in [computation]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc t h :: acc
  in revacc [as] [acc]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc t h :: acc
  in revacc as [acc]
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc t h :: acc
  in revacc as Nil
```

```
Reverses the supplied list.
reverse as :List A -> List A =
  let revacc as acc = case as of
    Nil -> acc
    Cons h t -> revacc t h :: acc
  in revacc as Nil
```

#### Editing lit/def/ref node

To initiate editing of the current node: RET, or maybe just start typing. This applies to literal
nodes or nodes that define or reference an identifier.

Edit span (changing literal or changing def/ref), RET commits edit, propagating change back to AST
(triggering retyping, which may cascade changes back down to markup).

#### Cursor and selection

Move cursor:
  * up/dn/lf/rt - navigate visually: move selection to find closest span in desired direction

Change selection:
  * shift up/dn - broaden selection: up moves up the AST, selecting enclosing expression; down
    moves down the AST (toward the cursor) selecting next narrower subexpression that contains the
    cursor.
