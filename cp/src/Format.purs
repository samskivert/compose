module Format where

import Control.Monad.State (State, modify, execState)
import Data.Array (length, snoc)
import Data.FoldableWithIndex (traverseWithIndex_)
import Prelude

import Constants (Constant(..))
import Markup as M
import Names (Name(..), toString)
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
-- | App Expr Expr => append spans
-- | + foo bar
-- |
-- | Let Name Type Expr Expr => append spans, merge nested lets, opt newline before in
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
-- | Abs Name Type Expr => append spans
-- | + x = body
-- |
-- | + x y = body  (nested abs)
-- |
-- | If Expr Expr Expr => append spans, opt newline before then & else
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
-- | CaseCase Expr Expr => append spans, opt format cexp to nested block
-- | + pat = cexp
-- |
-- | x pat =
-- |     cexp
-- |
-- | Case Expr (Array Expr) => append spans, format cases to nested block
-- | + case scrut of
-- |     casecase
-- |     casecase
-- |     ...
-- |
-- | x case scrut of casecase casecase ...

type AccLine = Array M.Span
type AccBlock = Array M.Elem
data Acc = Acc AccLine AccBlock

-- Operations:

-- - append span:
--   - adds span to acc line
appendSpan :: M.Span -> State Acc Unit
appendSpan span = do
  _ <- modify (addSpan span)
  pure unit
 where
  addSpan sp (Acc line block) = Acc (snoc line sp) block

-- - new line:
--   - adds acc line to acc block, sets acc line to []
--   - only if current acc line non-empty? otherwise noop?
newLine :: State Acc Unit
newLine = do
  _ <- modify commitLine
  pure unit
 where
  commitLine state @ (Acc line block) =
    if (length line == 0) then state
    else Acc [] (snoc block (M.Line line))

-- - append nested block:
--   - create new block (starts with blank acc line)
--   - format subexpr to nested block
--   - finalize nested block (append acc line if non-empty)
--   - do 'new line' operation
--   - append new block to acc block
appendBlock :: M.Elem -> State Acc Unit
appendBlock block = do
  _ <- modify (addBlock block)
  pure unit
 where
  addBlock bl (Acc aline ablock) = Acc aline (snoc ablock bl)

appendType :: T.Path -> Type -> State Acc Unit
appendType path tpe = case tpe of
  Unknown -> pure unit
  otherwise -> do
    -- appendSpan $ M.span_ " "
    appendSpan $ M.keySpan T.emptyPath ":"
    appendBareType path tpe
 where
  appendBareType kpath ktpe = case ktpe of
    Unknown -> pure unit
    Const const -> do
      appendSpan $ M.typeSpan kpath (show const)
    Data tag size -> do
      appendSpan $ M.typeSpan kpath (show tag <> show size)
    Arrow arg ret -> do
      appendBareType (T.extendPath kpath 0) arg
      appendSpan $ M.keySpan T.emptyPath " → "
      appendBareType (T.extendPath kpath 1) ret
    Ctor (Name name) -> do
      appendSpan $ M.typeSpan kpath name
    Var (Name name) -> do
      appendSpan $ M.typeSpan kpath name
    Apply ctor arg -> do
      appendBareType (T.extendPath kpath 0) ctor
      appendSpan $ M.span_ T.emptyPath " "
      appendBareType (T.extendPath kpath 1) arg

appendAbs :: T.Path -> Name -> Type -> T.Expr -> State Acc Unit
appendAbs path (Name arg) tpe body = case body of
  T.Abs arg1 tpe1 body1 -> do
    appendSpan $ M.defSpan (T.extendPath path 0) arg
    appendType (T.extendPath path 1) tpe
    appendSpan $ M.span_ T.emptyPath " "
    appendAbs (T.extendPath path 2) arg1 tpe1 body1
  otherwise -> do
    appendSpan $ M.defSpan (T.extendPath path 0) arg
    appendType (T.extendPath path 1) tpe
    appendSpan $ M.keySpan T.emptyPath " = "
    newLine
    appendBlock $ formatSubExpr (T.extendPath path 2) body

appendExpr :: T.Path -> T.Expr -> State Acc Unit
appendExpr path expr = case expr of
  T.Lit (Constant tag text) -> do
    appendSpan $ M.constantSpan path text
  T.Ref (Name name) -> do
    appendSpan $ M.identSpan path name
  T.Hole tpe -> do
    appendSpan $ M.holeSpan path
  T.App fexpr aexpr -> do
    appendExpr (T.extendPath path 0) fexpr
    appendSpan $ M.span_ T.emptyPath " "
    appendExpr (T.extendPath path 1) aexpr
  T.Let name tpe exp body -> do
    appendSpan $ M.keySpan path "let "
    appendAbs path name tpe exp
    newLine
    appendSpan $ M.keySpan T.emptyPath "in "
    appendExpr (T.extendPath path 2) body
  T.Abs arg tpe exp -> do
    appendAbs path arg tpe exp
  T.If test tt ff -> do
    appendSpan $ M.keySpan T.emptyPath "todo if"
  T.CaseCase pat exp -> do
    appendExpr (T.extendPath path 0) pat
    appendSpan $ M.keySpan T.emptyPath " → "
    appendExpr (T.extendPath path 1) exp
  T.Case scrut cases -> do
    appendSpan $ M.keySpan path "case "
    appendExpr (T.extendPath path 0) scrut
    appendSpan $ M.keySpan T.emptyPath " of"
    traverseWithIndex_ appendCase cases
 where
   appendCase idx cc = do
     newLine
     appendBlock $ formatSubExpr (T.extendPath path (idx + 1)) cc

appendField :: T.Path -> T.FieldDef -> State Acc Unit
appendField path { name, tpe } = do
  appendSpan $ M.identSpan (T.extendPath path 0) (toString name)
  appendType (T.extendPath path 0) tpe

appendRecord :: T.Path -> T.RecordDef -> State Acc Unit
appendRecord path { name, fields } = do
  appendSpan $ M.identSpan (T.extendPath path 0) (toString name)
  appendSpan $ M.keySpan T.emptyPath " ("
  traverseWithIndex_ appendField0 fields
  appendSpan $ M.keySpan T.emptyPath ")"
 where
  appendField0 idx ff = do
    appendBlock $ M.Block []
    -- TODO: comma separate, or newline separate if documented...

appendDef :: T.Path -> T.Def -> State Acc Unit
appendDef path def = case def of
  T.Term name expr -> do
    -- appendSpan $ M.keySpan "def "
    appendAbs path name Unknown expr
  T.Union (Name name) records -> do
    appendSpan $ M.keySpan T.emptyPath "data "
    appendSpan $ M.identSpan (T.extendPath path 0) name
    appendSpan $ M.keySpan T.emptyPath " ="
    newLine
    traverseWithIndex_ appendFormattedRecord records
  T.Record recdef -> do
    appendSpan $ M.keySpan (T.extendPath path 0) "data "
    appendRecord (T.extendPath path 1) recdef
 where
  appendFormattedRecord idx rr = do
    appendBlock $ formatRecord (T.extendPath path (idx+1)) rr
  formatRecord rpath rr = format (appendRecord rpath rr)

formatSubExpr :: T.Path -> T.Expr -> M.Elem
formatSubExpr path expr = format $ appendExpr path expr

formatDef :: T.Def -> M.Elem
formatDef def = format $ appendDef T.emptyPath def

formatExpr :: T.Expr -> M.Elem
formatExpr expr = format $ appendExpr T.emptyPath expr

format :: State Acc Unit -> M.Elem
format ff =
  let Acc _ block = execState op (Acc [] []) in M.Block block
  where
   op = do
     ff
     newLine
     -- TODO: assert that line is empty?

-- format :: T.Expr -> M.Defn
-- format expr = runPure $ run $ do
--   state <- new $ State [] []
--   pure { docs: [], code: [] }

-- Coalescing let and abs:
-- - let inspects body: if it's another let, append the let specially & 'lift' the body
-- - abs inspects body: if it's another abs, append the arg & 'lift' the body

-- State:
-- - current acc line
-- - current acc block
