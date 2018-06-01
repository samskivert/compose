module Editor where

import Control.Monad.State (modify_)
import Data.Array (mapWithIndex)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Web.Event.EventTarget as ET
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument as HD
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Markup as M
import Trees as T
import Format as F
import SpanEditor as SE

-- Cursor and selection model

-- | Models the editing cursor: indicates whether we're currently editing an editable leaf node
-- | (literal or name) and the path to that node. If we're not editing, the path identifies the node
-- | where editing will start if a "begin editing" operation is performed.
type Cursor = {path :: M.Path, editing :: Boolean}

nullCursor :: Cursor
nullCursor = {path: M.emptyPath, editing: false}

-- | Models the editing selection: a range of nodes that are selected (and visually highlighted) so
-- | that they may act as the target of an editing operation (like cut, extract into binding, etc.).
-- |
-- | The selection defaults to the AST node identified by the cursor but can be expanded to
-- | encompass more of the AST (generally by moving the selection 'up' the AST) or less (by moving
-- | the selection 'down' the AST toward the cursor).
-- |
-- | The cursor must always be inside the selection. When the cursor is moved, the selection is
-- | reset such that it starts and ends at the cursor's node.
type Selection = {start :: T.Path, end :: T.Path}

emptySelection :: Selection
emptySelection = {start: [], end: []}

-- | A span is either normal, selected or being edited.
data Mode = Normal | Selected | Edited
derive instance eqMode :: Eq Mode

testDefn :: M.Defn
testDefn =
  { docs: M.Para [ M.span_ T.emptyPath "Reverses the supplied list." ]
  , code: F.formatDef T.revExample
  }

testPath :: M.Path
testPath = M.mkPath [1, 0] 0

testState :: State
testState = State testDefn {path: testPath, editing: false} emptySelection

-- -----------------
-- Definition editor
-- -----------------

data Query a
  = Init a
  | HandleKey KeyboardEvent (H.SubscribeStatus -> a)
  | HandleSpan SE.Message a

data Message = Toggled Boolean

-- | The editor state is the def being edited, the current cursor, and the current selection.
data State = State M.Defn Cursor Selection

-- | TODO
data SpanSlot = SpanSlot
derive instance eqSpanSlot :: Eq SpanSlot
derive instance ordSpanSlot :: Ord SpanSlot

onKeyUp :: HD.HTMLDocument -> (KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
onKeyUp document fn = do
  let target = HD.toEventTarget document
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keyup listener false target
  pure $ ET.removeEventListener KET.keyup listener false target

handleKey :: KeyboardEvent -> State -> State
handleKey ev = case KE.code ev of
  "ArrowLeft"  -> moveCursor $ M.moveHoriz M.Left
  "ArrowRight" -> moveCursor $ M.moveHoriz M.Right
  "ArrowUp"    -> moveCursor $ M.moveVert M.Up
  "ArrowDown"  -> moveCursor $ M.moveVert M.Down
  "Enter"      -> toggleEdit
  _            -> identity
 where
  moveCursor mover (State {docs, code} {path, editing} sel) =
    let newPath = mover code path
    in State {docs, code} {path: newPath, editing: false} sel
  toggleEdit (State defn {path, editing} sel) =
    State defn {path, editing: true} sel

defnEditor :: H.Component HH.HTML Query Unit Message Aff
defnEditor = H.lifecycleParentComponent {
  initialState, initializer, finalizer, receiver, eval, render
} where
  initialState = const testState
  initializer = Just (H.action Init)
  finalizer = Nothing

  receiver = const Nothing

  eval :: Query ~> H.ParentDSL State Query SE.Query SpanSlot Message Aff
  eval (Init next) = do
    document <- H.liftEffect $ DOM.document =<< DOM.window
    H.subscribe $ ES.eventSource' (onKeyUp document) (Just <<< H.request <<< HandleKey)
    pure next
  eval (HandleKey ev reply) = trace ev \_ -> do
    modify_ $ handleKey ev
    pure (reply H.Listening)
  eval (HandleSpan (SE.CommitEdit span) next) = trace span \_ -> do
    pure next

  render :: State -> H.ParentHTML Query SE.Query SpanSlot Aff
  render (State {docs, code} curs sel) = HH.div
    [HP.classes [HH.ClassName "defn"]]
    [renderElem curs docs, renderElem curs code]

  followCursor {path, editing} idx = M.popPath nullCursor {path: _, editing} idx path

  renderElems curs elems =
    mapWithIndex (\idx elem -> renderElem (followCursor curs idx) elem) elems

  renderElem curs elem = case elem of
    M.Block elems -> HH.div [HP.classes [HH.ClassName "block"]] (renderElems curs elems)
    M.Para  spans -> HH.div [HP.classes [HH.ClassName "docs"]]  (renderSpans curs spans)
    M.Line  spans -> HH.div []                                  (renderSpans curs spans)

  renderSpans {path, editing} spans =
    let mode idx = if not (M.isLeaf path && (idx == path.span)) then Normal
                   else if editing then Edited else Selected
    in mapWithIndex (\idx span -> renderSpan (mode idx) span) spans

  renderSpan mode span @ {text, styles, path} = case mode of
    Edited -> HH.slot SpanSlot (SE.spanEditor span) unit (HE.input HandleSpan)
    _ ->
      let sstyles = if (mode == Selected) then styles <> ["selected"] else styles
      in HH.span [HP.classes (HH.ClassName <$> sstyles), HP.title $ show path] [HH.text text]
