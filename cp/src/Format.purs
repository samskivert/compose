module Format where

import Control.Monad.ST (ST, run, foreach)
import Control.Monad.ST.Ref (STRef, new, read, modify)
import Data.Array (length, snoc)
import Prelude

import Constants (Constant(..))
import Markup as M
import Names (Name(..))
import Trees as T
import Types (Type(..))

-- | Lit Constant => append span
-- | + "hello"
-- | + 42
-- | + false
-- |
-- | Ref Name => append span
-- | + foo
-- |
-- | App Tree Tree => append spans
-- | + foo bar
-- |
-- | Let Name Type Tree Tree => append spans, merge nested lets, opt newline before in
-- | + let name = exp in body
-- |
-- | + let name = exp
-- |   in  body
-- |
-- | + let foo = fooexp  (nested let)
-- |       bar = barexp
-- |   in  body
-- |
-- | x let
-- |     foo = fooexp
-- |     bar = barexp
-- |   in
-- |     body
-- |
-- | Abs Name Type Tree => append spans
-- | + x = body
-- |
-- | + x y = body  (nested abs)
-- |
-- | If Tree Tree Tree => append spans, opt newline before then & else
-- | + if test then texp else fexp
-- |
-- | + if test
-- |   then texp
-- |   else fexp
-- |
-- | x if
-- |     test
-- |   then
-- |     texp
-- |   else
-- |     fexp
-- |
-- | CaseCase Tree Tree => append spans, opt format cexp to nested block
-- | + pat = cexp
-- |
-- | x pat =
-- |     cexp
-- |
-- | Case Tree (Array Tree) => append spans, format cases to nested block
-- | + case scrut of
-- |     casecase
-- |     casecase
-- |     ...
-- |
-- | x case scrut of casecase casecase ...

type AccLine = Array M.Span
type AccBlock = Array M.CodeElem
data State = State AccLine AccBlock

-- Operations:

-- - append span:
--   - adds span to acc line
addSpan :: M.Span -> State -> State
addSpan span (State line block) = State (snoc line span) block

appendSpan :: forall h. STRef h State -> M.Span -> ST h Unit
appendSpan stref span = do
  _ <- modify (addSpan span) stref
  pure unit

-- - new line:
--   - adds acc line to acc block, sets acc line to []
--   - only if current acc line non-empty? otherwise noop?
commitLine :: State -> State
commitLine state @ (State line block) =
  if (length line == 0) then state
  else State [] (snoc block (M.Line line))

newLine :: forall h. STRef h State -> ST h Unit
newLine stref = do
  _ <- modify commitLine stref
  pure unit

addBlock :: M.CodeElem -> State -> State
addBlock block (State aline ablock) = State aline (snoc ablock block)

appendBlock :: forall h. STRef h State -> M.CodeElem -> ST h Unit
appendBlock stref block = do
  _ <- modify (addBlock block) stref
  pure unit

-- - append nested block:
--   - create new block (starts with blank acc line)
--   - format subexpr to nested block
--   - finalize nested block (append acc line if non-empty)
--   - do 'new line' operation
--   - append new block to acc block

appendType :: forall h. STRef h State -> Type -> ST h Unit
appendType stref tpe = case tpe of
  Unknown -> pure unit
  otherwise -> do
    appendSpan stref $ M.span_ " "
    appendSpan stref $ M.keySpan ":"
    appendBareType tpe
  where
    appendBareType tpe = case tpe of
      Unknown -> pure unit
      Const const -> do
        appendSpan stref $ M.typeSpan $ (show const)
      Data tag size -> do
        appendSpan stref $ M.typeSpan $ (show tag) <> (show size)
      Arrow arg ret -> do
        appendBareType arg
        appendSpan stref $ M.keySpan " -> "
        appendBareType ret
      Ctor (Name name) -> do
        appendSpan stref $ M.typeSpan name
      Var (Name name) -> do
        appendSpan stref $ M.typeSpan name
      Apply ctor arg -> do
        appendBareType ctor
        appendSpan stref $ M.span_ " "
        appendBareType arg

appendAbs :: forall h. STRef h State -> Name -> Type -> T.Tree -> ST h Unit
appendAbs stref (Name arg) tpe body = case body of
  T.Abs arg1 tpe1 body1 -> do
    appendSpan stref $ M.defSpan arg
    appendType stref tpe
    appendSpan stref $ M.span_ " "
    appendAbs stref arg1 tpe1 body1
  otherwise -> do
    appendSpan stref $ M.defSpan arg
    appendType stref tpe
    appendSpan stref $ M.keySpan " = "
    newLine stref
    appendBlock stref $ M.Block (formatTree body)

appendTree :: forall h. STRef h State -> T.Tree -> ST h Unit
appendTree stref tree = do
  case tree of
    T.Def name body -> do
      -- appendSpan stref $ M.keySpan "def "
      appendAbs stref name Unknown body
    T.Lit (Constant tag text) -> appendSpan stref $ M.constantSpan text
    T.Ref (Name name) -> appendSpan stref $ M.identSpan name
    T.App ftree atree -> do
      appendTree stref ftree
      appendSpan stref $ M.span_ " "
      appendTree stref atree
    T.Let name tpe exp body -> do
      appendSpan stref $ M.keySpan "let "
      appendAbs stref name tpe exp
      newLine stref
      appendSpan stref $ M.keySpan "in "
      appendTree stref body
    T.Abs arg tpe exp -> appendAbs stref arg tpe exp
    T.If test tt ff -> appendSpan stref $ M.keySpan "todo if"
    T.CaseCase pat exp -> do
      appendTree stref pat
      appendSpan stref $ M.keySpan " -> "
      appendTree stref exp
    T.Case scrut cases -> do
      appendSpan stref $ M.keySpan "case "
      appendTree stref scrut
      appendSpan stref $ M.keySpan " of"
      foreach cases \cc -> do
        newLine stref
        appendBlock stref $ M.Block (formatTree cc)

formatTree :: T.Tree -> Array M.CodeElem
formatTree tree = run do
  stref <- new $ State [] []
  appendTree stref tree
  newLine stref
  State _ block <- read stref
-- TODO: assert that line is empty?
  pure $ block

-- format :: T.Tree -> M.Defn
-- format tree = runPure $ run $ do
--   state <- new $ State [] []
--   pure { docs: [], code: [] }

-- Coalescing let and abs:
-- - let inspects body: if it's another let, append the let specially & 'lift' the body
-- - abs inspects body: if it's another abs, append the arg & 'lift' the body

-- State:
-- - current acc line
-- - current acc block
