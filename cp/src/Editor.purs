module Editor where

import Control.Monad.State (modify_)
import Data.Array (mapWithIndex)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
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

-- -----------------
-- Definition editor
-- -----------------

data Query a
  = Init a
  | HandleKey KeyboardEvent (H.SubscribeStatus -> a)
  | HandleSpan SE.Message a

data Message = Toggled Boolean

-- | The editor state is the def being edited, the current cursor, and the current selection.
data State = State T.Def M.Elem Cursor Selection

mkState :: T.Def -> State
mkState def =
  let elem = F.formatDef def
  in State def elem {path: (M.mkPath (M.firstEditable elem) 0), editing: false} emptySelection

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
  moveCursor mover (State def elem {path, editing} sel) =
    let newPath = mover elem path
    in State def elem {path: newPath, editing: false} sel
  toggleEdit (State def elem {path, editing} sel) =
    trace "toggleEdit" \_ -> State def elem {path, editing: true} sel

handleEditingKey ev = case KE.code ev of
  "Enter" -> do
    op <- H.query SpanSlot $ H.request SE.CommitEdit
    modify_ $ applyEdit (fromMaybe identity op)
    pure unit
  _ -> pure unit

applyEdit :: (T.Def -> T.Def) -> State -> State
applyEdit op (State def elem {path, editing} sel) =
  let ndef = op def
  in trace "applyEdit" \_ -> State ndef (F.formatDef ndef) {path, editing: false} sel

defEditor :: T.Def -> H.Component HH.HTML Query Unit Message Aff
defEditor initDef  = H.lifecycleParentComponent {
  initialState, initializer, finalizer, receiver, eval, render
} where
  initialState = const $ mkState initDef
  initializer = Just (H.action Init)
  finalizer = Nothing

  receiver = const Nothing

  eval :: Query ~> H.ParentDSL State Query SE.Query SpanSlot Message Aff
  eval (Init next) = do
    document <- H.liftEffect $ DOM.document =<< DOM.window
    H.subscribe $ ES.eventSource' (onKeyUp document) (Just <<< H.request <<< HandleKey)
    pure next
  eval (HandleKey ev reply) = trace ev \_ -> do
    State _ _ {path, editing} _ <- H.get
    _ <- if editing then handleEditingKey ev
         else modify_ $ handleKey ev
    pure (reply H.Listening)
  eval (HandleSpan (SE.DisabledCommitEdit op) next) = trace op \_ -> do
    modify_ (applyEdit op)
    pure next

  render :: State -> H.ParentHTML Query SE.Query SpanSlot Aff
  render (State def elem curs sel) = HH.div
    [HP.classes [HH.ClassName "def"]]
    [renderElem curs elem]

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

  renderSpan mode span @ {text, styles, editor} = case mode of
    Edited -> HH.slot SpanSlot (SE.spanEditor span) unit (HE.input HandleSpan)
    _ ->
      let sstyles = if (mode == Selected) then styles <> ["selected"] else styles
      in HH.span [HP.classes (HH.ClassName <$> sstyles)] [HH.text text]
