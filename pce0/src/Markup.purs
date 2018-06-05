module Markup where

import Control.Monad.State as CMS
import Control.MonadZero (guard)
import Data.Array ((!!), snoc, uncons, unsnoc, null, head, last, length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String.Common (joinWith)
import Debug.Trace (trace)
import Effect (Effect, foreachE)
import Effect.Class.Console (log)
import Prelude

import Trees as T

-- ------------
-- Markup model
-- ------------

-- The AST is transformed into a markup model which is used to display the code in a familiar
-- "syntax highlighted, formatted text" format, but the text itself is not editable. Edits are made
-- on the underlying AST and those changes propagate back out to the visualization.

-- | A function used to update a `Def` tree based on the new text provided for a span.
type EditFn = String -> T.Def -> T.Def

-- | A sequence of characters that are displayed in a single style (typeface, weight, color, etc.).
-- | If the span represents a component of an AST node, its path will reflect the path to that node
-- | (from the AST root used to generate the markup). Otherwise the path will be `Nil`.
type Span = {
  text :: String,
  styles :: Array String,
  editor :: Maybe EditFn
  -- TODO: other stuff like completion functions, indications on how many trailing spans
  -- get slurped up if we edit this span, etc.
}

showSpan :: Span -> String
showSpan {text, styles, editor} = case editor of
  Just _ -> "[" <> text <> "]"
  Nothing -> "<" <> text <> ")"

-- TODO: do we want a separate visualization model for docs?
-- could allow the HTML layout engine to handle wrapping...

-- | Code is marked up as elements that are either blocks or lines. A block aggregates lines and is
-- | indented relative to its enclosing block, and a line aggregate spans. Note that wrapping of
-- | spans is done during code formatting. We do not allow the HTML layout engine to wrap code.
-- |
-- | Documentation is marked up into elements that are either paragraphs or code blocks. A paragraph
-- | is a collection of spans that _is_ allowed to be wrapped by the HTML layout engine. A code
-- | block is just formatted (and editable) code that can be interspersed between paragraphs.
-- |
-- | TODO: do we need special support for inline code identifiers?
data Elem = Block (Array Elem) | Para (Array Span) | Line (Array Span)

-- derive instance genericElem :: Generic Elem _
-- instance showElem :: Show Elem where
--   show elem = genericShow elem

-- | Creates a span containing `text` with `styles`.
span :: Array String -> Maybe EditFn -> String -> Span
span styles editor text = {text, styles, editor}

-- | Creates a span containing `text` and no styles.
span_ :: Maybe EditFn -> String -> Span
span_ = span []

-- ----------------------------
-- Styled spans for code markup
-- ----------------------------

constantSpan :: Maybe EditFn -> String -> Span
constantSpan = span ["constant"]

identSpan :: Maybe EditFn -> String -> Span
identSpan = span ["ident"]

defSpan :: Maybe EditFn -> String -> Span
defSpan = span ["ident", "def"]

keySpan :: Maybe EditFn -> String -> Span
keySpan = span ["keyword"]

typeSpan :: Maybe EditFn -> String -> Span
typeSpan = span ["type"]

holeSpan :: Maybe EditFn -> Span
holeSpan editor = span ["hole"] editor "?"

-- ----------
-- Path model
-- ----------

-- We use paths to keep track of the cursor when editing a marked up AST. Given a markup tree (Elem)
-- and a path, we can move the cursor left, right, up, down, based on the visual structure of the
-- markup.

-- | Identifies a path from some root Elem to a Span. `idxs` indicates the indices of each nested
-- | `Block` starting from the root of the markup tree. The final component of `idxs` identifies a
-- | `Para` or `Line`. `span` is the index of the addressed span inside the addressed para or line.
type Path = {idxs :: Array Int, span :: Int}

emptyPath :: Path
emptyPath = {idxs: [], span: -1}

mkPath :: Array Int -> Int -> Path
mkPath idxs span = {idxs, span}

isLeaf :: Path -> Boolean
isLeaf {idxs, span} = null idxs

-- | Removes the top component of a path and applies `op` to the remainder. If the top of the path
-- | is not equal to `idx`, `dflt` is returned instead.
popPath :: forall a. a -> (Path -> a) -> Int -> Path -> a
popPath dflt op idx path = fromMaybe dflt $ do
  {head:top, tail:rest} <- uncons path.idxs
  guard $ top == idx
  pure $ op path {idxs = rest}

xelem :: Elem -> Array Int -> Elem
xelem elem [] = elem
xelem elem idxs = case elem of
  Block elems -> fromMaybe elem $ do
    {head:idx, tail:rest} <- uncons idxs
    celem <- elems !! idx
    pure $ xelem celem rest
  Para _ -> elem
  Line _ -> elem

-- | Extracts the spans addressed by `idxs` from the tree rooted at `elem`.
-- | Yields `[]` if the path is invalid and does not address a `Line` or `Para`.
spans :: Elem -> Array Int -> Array Span
spans elem idxs = case elem of
  Block elems -> fromMaybe [] $ do
    {head:idx, tail:rest} <- uncons idxs
    celem <- elems !! idx
    pure $ spans celem rest
  Para spans -> spans
  Line spans -> spans

-- | Used to "move" a path left or right. See `moveHoriz`.
data HDir = Left | Right

deltaH :: HDir -> Int
deltaH Left  = -1
deltaH Right =  1

moveHoriz :: HDir -> Elem -> Path -> Path
moveHoriz dir relem {idxs, span} = {idxs, span: moveSpanH dir span (spans relem idxs)}

moveSpanH :: HDir -> Int -> Array Span -> Int
moveSpanH dir idx spans = scan spans idx where
  scan spans cidx = fromMaybe idx $ do
    let nidx = cidx + deltaH dir
    span <- spans !! nidx
    pure $ if isNothing span.editor then scan spans nidx else nidx

-- | Used to "move" a path up or down. See `moveVert`.
data VDir = Up | Down

deltaV :: VDir -> Int
deltaV Up   = -1
deltaV Down =  1

moveVert :: VDir -> Elem -> Path -> Path
moveVert Down relem rpath@{idxs, span} =
  let fidxs = forward idxs          -- move forward to the next valid path
      felem = xelem relem fidxs     -- obtain the element at that path position
      suff  = firstEditable felem   -- get the path from that element to the first line/para
      vpath = fidxs <> suff         -- combine the path prefix and suffix into a full path
  in {idxs: vpath, span: 0}
  where
    -- try to move forward in the current element, otherwise move up
    -- the path one component and then try to move forward there
    forward idxs = unsnoc idxs <#> check # fromMaybe idxs
    check {init, last} = case xelem relem init of
      Block elems -> if (last < lastidx elems) then (snoc init (last+1)) else forward init
      _           -> idxs
moveVert Up   relem rpath@{idxs, span} =
  let bidxs = backup idxs           -- move backward to the next valid path
      lelem = xelem relem bidxs     -- obtain the element at that path position
      suff = lastEditable lelem     -- get the path from that element to the first line/para
  in {idxs: bidxs <> suff, span: 0} -- combine the path prefix and suffix into a full path
  where
    -- try to move backward in the current element, otherwise move up
    -- the path one component and then try to move backward there
    backup idxs = unsnoc idxs <#> check # fromMaybe idxs
    check {init, last} = if (last > 0) then (snoc init (last-1)) else backup init

lastidx :: forall a. Array a -> Int
lastidx elems = length elems - 1

firstEditable :: Elem -> Array Int
firstEditable = findEditable (const 0)

lastEditable :: Elem -> Array Int
lastEditable = findEditable lastidx

findEditable :: (Array Elem -> Int) -> Elem -> Array Int
findEditable pickelem = loop []
  where
  loop suff = case _ of
    Block elems -> let idx = pickelem elems
                   in elems !! idx <#> loop (snoc suff idx) # fromMaybe []
    Para  _     -> suff
    Line  _     -> suff

-- --------------
-- Debug printing
-- --------------

debugShowElem :: Elem -> Array String
debugShowElem elem = debugShow "" elem
 where
  debugShow indent = case _ of
    Block elems -> elems >>= (debugShow (indent <> "  "))
    Para  spans -> debugShowSpans indent spans
    Line  spans -> debugShowSpans indent spans
  debugShowSpans indent spans = [indent <> (joinWith "" (spans <#> (_.text)))]

logElem :: Elem -> Effect Unit
logElem elem = foreachE (debugShowElem elem) log
