module SpanEditor where

import Data.Maybe (Maybe(..), maybe)
import Debug.Trace (trace)
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Element as DE
import Web.UIEvent.KeyboardEvent as KE

import Markup as M
import Trees as T

data Query a
  = NoteInput (Maybe DE.Element) a
  | Init a
  | UpdateText String a
  | CommitText a
  | CommitEdit ((T.Def -> T.Def) -> a)

data Message
  = DisabledCommitEdit (T.Def -> T.Def)

spanEditor :: forall m. M.Span -> H.Component HH.HTML Query Unit Message m
spanEditor {text: initText, editor} =
  H.lifecycleComponent
    { initialState: const initText
    , initializer
    , finalizer
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initializer = Just (H.action Init)
  finalizer = Nothing

  render :: String -> H.ComponentHTML Query
  render text = HH.input
    [ HP.type_ HP.InputText
    -- , HC.ref \el -> Just $ H.action (NoteInput el)
    , HP.placeholder "..."
    , HP.autofocus true
    , HP.value text
    , HE.onValueInput (HE.input UpdateText)
    -- , HE.onKeyDown \e -> case KE.code e of
    --     "Enter" -> Just (H.action CommitText)
    --     _       -> Nothing
    -- , HE.onBlur (HE.input_ CommitText)
    ]

  eval :: Query ~> H.ComponentDSL String Query Message m
  eval (NoteInput el next) = trace el \_ -> do
    pure next
  eval (Init next) = trace "init!" \_ -> do
    -- TODO: request focus on el
    pure next
  eval (UpdateText text next) = do
    H.put text
    pure next
  eval (CommitText next) = do
    text <- H.get
    H.raise $ DisabledCommitEdit (maybe identity (\editfn -> editfn text) editor)
    pure next
  eval (CommitEdit reply) = do
    text <- H.get
    pure (reply $ maybe identity (\editfn -> editfn text) editor)
