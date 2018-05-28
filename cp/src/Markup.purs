module Markup where

import Trees (Tree)

-- Markup model

-- The AST is transformed into a markup model which is used to display the code in a familiar
-- "syntax highlighted, formatted text" format, but the text itself is not editable. Edits are made
-- on the underlying AST and those changes propagate back out to the visualization.

-- | A sequence of characters that are displayed in a single style (typeface, weight, color, etc.).
-- | If a span is editable, it is displayed using an editable text field. Otherwise it is displayed
-- | as read-only text.
type Span = { text :: String, styles :: Array String, editable :: Boolean }

-- TODO: do we want a separate visualization model for docs?
-- could allow the HTML layout engine to handle wrapping...

-- | Code is marked up as elements that are either blocks or lines. A block aggregates lines and is
-- | indented relative to its enclosing block, and a line aggregate spans. Note that wrapping of
-- | spans is done during code formatting. We do not allow the HTML layout engine to wrap code.
data CodeElem = Block (Array CodeElem) | Line (Array Span)

-- | Documentation is marked up into elements that are either paragraphs or code blocks. A paragraph
-- | is a collection of spans that _is_ allowed to be wrapped by the HTML layout engine. A code
-- | block is just formatted (and editable) code that can be interspersed between paragraphs.
-- | TODO: do we need special support for inline code identifiers?
data DocElem = Para (Array Span) | Code CodeElem

-- | A definition combines documentation and code.
type Defn = { docs :: Array DocElem, code :: Array CodeElem }

-- | Create a simple span containing `text` with `styles`.
span :: Array String -> String -> Span
span styles text = { text, styles, editable: false }

-- | Create a simple span containing `text` and no styles.
span_ :: String -> Span
span_ = span []

-- Styled spans for code markup

constantSpan :: String -> Span
constantSpan = span ["constant"]

identSpan :: String -> Span
identSpan = span ["ident"]

defSpan :: String -> Span
defSpan = span ["ident", "def"]

keySpan :: String -> Span
keySpan = span ["keyword"]

typeSpan :: String -> Span
typeSpan = span ["type"]

-- Cursor and selection model

-- The markup model is to display the code, and a cursor and selection model are used to control
-- editing operations and visualize where they will take effect.

-- | A path through the AST from the root of the top-level definition to a leaf node.
-- TODO: should be a List with child node at the head?
type Path = Array Tree

-- | Models the editing cursor: indicates whether we're currently editing an editable leaf node
-- | (literal or name) and the path to that node. If we're not editing, the path identifies the node
-- | where editing will start if a "begin editing" operation is performed.
type Cursor =
  { path :: Path
  , editing :: Boolean
  }

-- | Models the editing selection: a range of nodes that are selected (and visually highlighted) so
-- | that they may act as the target of an editing operation (like cut, extract into binding, etc.).
-- | The cursor must always be inside the selection. When the cursor is moved, the selection is
-- | reset such that it starts and ends at the cursor's node.
type Selection =
  { start :: Path
  , end :: Path
  }
