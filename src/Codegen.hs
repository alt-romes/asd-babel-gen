{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Codegen where

import qualified Data.Map as M

import Data.Functor.Foldable

import Control.Monad
import Control.Monad.RWS.CPS

import Language.Java.Syntax hiding (Assign, NotEq)
import qualified Language.Java.Syntax as J

import Syntax

type Env = M.Map Identifier ()
type Babel = RWS Env () ()

translate :: Algorithm Typed -> Babel J.CompilationUnit
translate p@(P (StateD varTypes vars) tops) =
  let
      fieldDecls = map (\(v, t) -> MemberDecl $ FieldDecl [Private] t [VarDecl (VarId (Ident v)) Nothing]) (zip vars varTypes)
   in
      local (\r -> foldr (\v -> M.insert v ()) r vars) do -- add vars to env
        methodDecls <- forM tops $ \case
          UponD argTypes name args stmts -> do
            bodyStmts <- mapM translateStmt stmts
            pure $ MemberDecl $ MethodDecl [Public] [] Nothing (Ident name) (map (\(a, t) -> FormalParam [] t False (VarId (Ident a))) (zip args argsTypes)) [] Nothing (MethodBody $ Just $ Block bodyStmts)
        let classBody   = ClassBody $ fieldDecls <> methodDecls
        pure $ CompilationUnit Nothing [] [ClassTypeDecl $ ClassDecl [Public] (Ident "ClassName") [] Nothing [] classBody]

translateStmt :: Statement Typed -> Babel BlockStmt
translateStmt = fmap BlockStmt . cata \case
  AssignF i e -> do
    e' <- translateExp e
    pure $ ExpStmt $ J.Assign (NameLhs $ Name [Ident i]) EqualA e'

  IfF e thenS elseS -> do
    e' <- translateExp e
    thenS' <- StmtBlock . Block . map BlockStmt <$> sequence thenS
    case elseS of
      [] ->
        pure $ IfThen e' thenS'
      _  -> do
        elseS' <- StmtBlock . Block . map BlockStmt <$> sequence elseS
        pure $ IfThenElse e' thenS' elseS'

  TriggerF i args -> do
    -- TODO: If the trigger is outside of scope this should be a triggerNotify instead
    args' <- mapM translateExp args
    pure (ExpStmt $ MethodInv $ MethodCall (Name [Ident i]) args')
  TriggerSendF msgType host args -> _

  ForeachF _ name e body -> do
    e' <- translateExp e
    body' <- sequence body
    pure $ EnhancedFor [] _type (Ident name) e' (StmtBlock . Block . map BlockStmt $ body')


translateExp :: Expr -> Babel Exp
translateExp = cata \case
  IF i -> pure $ Lit $ Int i
  BF b -> pure $ Lit $ Boolean b
  IdF i -> pure $ ExpName $ Name [Ident i]
  InF e1 e2 -> do
    e1' <- e1
    e2' <- e2
    pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "contains") [e2']
  NotInF e1 e2 -> do
    e1' <- e1
    e2' <- e2
    pure $ PreNot $ MethodInv $ PrimaryMethodCall e1' [] (Ident "contains") [e2']
  EqF e1 e2 -> do
    e1' <- e1
    e2' <- e2
    pure $ BinOp e1' Equal e2'
  NotEqF e1 e2 -> do
    e1' <- e1
    e2' <- e2
    pure $ BinOp e1' J.NotEq e2'
  SetF s -> _
  MapF m -> _
  UnionF e1 e2 -> do
    -- TODO: Only works for sets yet, but should also work for maps.
    e1' <- e1
    e2' <- e2
    pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "addAll") [e2']
  DifferenceF e1 e2 -> do
    -- TODO: Only works for sets yet, but should also work for maps.
    e1' <- e1
    e2' <- e2
    pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "removeAll") [e2']

