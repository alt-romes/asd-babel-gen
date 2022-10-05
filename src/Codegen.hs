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

translateAlg :: Algorithm Typed -> Babel J.CompilationUnit
translateAlg (P (StateD varTypes vars) tops) = do
  varTypes' <- mapM translateType varTypes
  let
      fieldDecls = map (\(v, t) -> MemberDecl $ FieldDecl [Private] t [VarDecl (VarId (Ident v)) Nothing]) (zip vars varTypes')
   in
      local (\r -> foldr (\v -> M.insert v ()) r vars) do -- add vars to env
        methodDecls <- forM tops $ \case
          UponD argTypes name args stmts -> do
            bodyStmts <- mapM translateStmt stmts
            argTypes' <- mapM translateType argTypes
            pure $ MemberDecl $ MethodDecl [Public] [] Nothing (Ident name) (map (\(a, t) -> FormalParam [] t False (VarId (Ident a))) (zip args argTypes')) [] Nothing (MethodBody $ Just $ Block bodyStmts)
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
  TriggerSendF msgType host args -> error "TriggerSend translate"

  ForeachF t name e body -> do
    e' <- translateExp e
    body' <- sequence body
    t' <- translateType t
    pure $ EnhancedFor [] t' (Ident name) e' (StmtBlock . Block . map BlockStmt $ body')


translateExp :: Expr Typed -> Babel Exp
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
  SetF t s -> do
    translateType t >>= \case
      PrimType _ -> error "PrimType (Non-RefType) Set"
      RefType t' -> pure $ InstanceCreation [ActualType t'] (TypeDeclSpecifierUnqualifiedWithDiamond (Ident "HashSet") Diamond) [] Nothing
  MapF _ m -> error "Map translate"
  UnionF t e1 e2 -> do
    -- TODO: Only works for sets yet, but should also work for maps.
    e1' <- e1
    e2' <- e2
    pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "addAll") [e2']
  DifferenceF t e1 e2 -> do
    -- TODO: Only works for sets yet, but should also work for maps.
    e1' <- e1
    e2' <- e2
    pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "removeAll") [e2']

translateType :: AType -> Babel Type
translateType = cata \case
  TIntF -> pure $ PrimType IntT
  TBoolF -> pure $ PrimType BooleanT
  TStringF -> pure $ RefType $ ClassRefType $ ClassType [(Ident "String", [])]
  TSetF x -> x >>= \case
    PrimType x' -> error $ show x' <> " is not a boxed/Object type"
    RefType t  -> pure $ RefType $ ClassRefType $ ClassType [(Ident "Set", [ActualType t])]
  TMapF x -> x >>= \case
    PrimType x' -> error $ show x' <> " is not a boxed/Object type"
    RefType t  -> pure $ RefType $ ClassRefType $ ClassType [(Ident "Map", [ActualType t])]
  TUnknownF -> error "Unknown type"
  TVoidFunF _ -> error "Fun type"
  TVarF i -> error $ "Type variable " <> show i
  
