module SpanEditor where

import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KE

import Markup as M
import Trees as T
import Format as F

data Query a
  = UpdateText String a
  | CommitText a

data Message
  = CommitEdit M.Span

spanEditor :: forall m. M.Span -> H.Component HH.HTML Query Unit Message m
spanEditor initSpan =
  H.component
    { initialState: const initSpan
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: M.Span -> H.ComponentHTML Query
  render span =
    trace ("render: " <> show span) \_ -> HH.input
      [ HP.type_ HP.InputText
      , HP.placeholder "..."
      , HP.autofocus true
      , HP.value span.text
      , HE.onValueInput (HE.input UpdateText)
      , HE.onKeyDown \e -> case KE.code e of
          "Enter" -> Just (H.action CommitText)
          _       -> Nothing
      , HE.onBlur (HE.input_ CommitText)
      ]

  eval :: Query ~> H.ComponentDSL M.Span Query Message m
  eval (UpdateText text next) = trace text \_ -> do
    H.modify_ (_ {text=text})
    pure next
  eval (CommitText next) = do
    span <- H.get
    H.raise $ CommitEdit span
    pure next
