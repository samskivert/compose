module Editor where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)
import Data.Array (length, snoc)

import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Markup as M
import Trees as T
import Format as F

data Query a
  = Toggle a

data Message = Toggled Boolean

-- | The cursor identifies the AST node either being edited or which will be edited if an edit
-- | operation is initiated. This is generally the deepest possible node (i.e. lit or name ref).
type Cursor = T.Tree

-- | The selection identifies the AST node that is visualized as the selection and which will be cut
-- | or copied if a cut or copy action is performed. This defaults to the same AST node as the
-- | cursor but can be expanded to encompass more of the AST (generally by moving the selection 'up'
-- | the AST) or less (by moving the selection 'down' the AST toward the cursor).
type Selection = T.Tree

-- | The editor state is the tree being edited, the current cursor and the current selection.
data State = State M.Defn Cursor Selection

spaceSep :: Array M.Span -> Array M.Span
spaceSep spans =
  let addSpace acc s = if length acc == 0 then [s] else acc <> [M.span_ " ", s]
  in foldl addSpace [] spans

textLine :: Array String -> M.CodeElem
textLine words = M.Line $ spaceSep $ map M.span_ words

testState :: M.Defn
testState =
  { docs:
      [ M.Para [ M.span_ "Reverses the supplied list." ]
      ]
  -- , code:
  --     [ textLine [ "reverse", "as", ":List", "A", "→", "List", "A", "=" ]
  --     , M.Block
  --         [ textLine [ "let", "revacc", "as", "acc", "=", "case", "as", "of" ]
  --         , M.Block
  --             [ textLine [ "Nil", "→", "acc" ]
  --             , textLine [ "Cons", "h", "t", "→", "revacc", "t", "h", "::", "acc" ]
  --             ]
  --         , textLine [ "in", "revacc", "as", "Nil" ]
  --         ]
  --     ]
  , code: F.formatTree T.revExample
  }

defnEditor :: forall m. H.Component HH.HTML Query Unit Message m
defnEditor = H.component { initialState, render, eval, receiver } where
  initialState = const testState

  render :: M.Defn -> H.ComponentHTML Query
  render { docs, code } = HH.div
    [ HP.classes [HH.ClassName "defn"] ]
    (snoc (renderDoc <$> docs) (HH.div [HP.classes [HH.ClassName "code"]] (renderCode <$> code)))

  eval :: Query ~> H.ComponentDSL M.Defn Query Message m
  eval = case _ of
    Toggle next -> do
      -- state <- H.get
      -- let nextState = not state
      -- H.put nextState
      -- H.raise (Toggled nextState)
      pure next

  receiver = const Nothing

  renderDoc doc = case doc of
    M.Para spans -> HH.div [HP.classes [HH.ClassName "docs"]] (renderSpan <$> spans)
    M.Code code  -> renderCode code

  renderCode code = case code of
    M.Block lines -> HH.div [HP.classes [HH.ClassName "block"]] (renderCode <$> lines)
    M.Line spans  -> renderLine spans

  renderLine spans = HH.div_ (renderSpan <$> spans)
  renderSpan { text, editable, styles } = HH.span
    [ HP.classes (HH.ClassName <$> styles) ]
    [ HH.text text ]
