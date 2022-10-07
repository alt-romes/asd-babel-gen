{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Typechecker where

-- TODO: Really, we should only do this after passing through an initial desugaring phase into another intermediate representation.
-- Since it doesn't really matter in the long term, I'll just accept this mess

import Data.Coerce
import qualified Data.Char as C
import qualified Data.Set as S
import qualified Data.Map as M

import Data.Functor.Foldable

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.State
import Control.Monad.RWS.CPS

import Syntax

type Infer = RWS REnv [Constraint] Uniq
type REnv = [M.Map Identifier AType]
type Constraint = (AType, AType)
newtype Uniq = Uniq Int

typecheck :: Algorithm Parsed -> Either TypeError (Algorithm Typed)
typecheck alg = case runInfer (inferAlg alg) of
  (alg', constraints) -> flip applySubst alg' <$> solve constraints
    

inferAlg :: Algorithm Parsed -> Infer (Algorithm Typed)
inferAlg (P i (StateD _ vars) tops) = do
  freshTVars <- mapM (const (TVar <$> fresh)) vars
  freshTopTVars <- mapM (const (TVar <$> fresh)) tops
  topsT <- pushScope (zip vars freshTVars <> zip (map topIdent tops) freshTopTVars) $ mapM inferTop tops
  pure (P i (StateD freshTVars vars) topsT)
    where
      topIdent :: TopDecl Parsed -> Identifier
      topIdent = \case
        UponD _ n args _
          | map C.toLower n == "receive" -> head args
          | otherwise -> n

inferTop :: TopDecl Parsed -> Infer (TopDecl Typed)
inferTop = \case
  UponD _ name args stmts
    | map C.toLower name == "receive" -> do
      freshTVars <- mapM (const (TVar <$> fresh)) (drop 2 args)
      let argsTypes = TMessageType:TClass "Host":freshTVars
      stmtsT <- pushScope (zip (drop 1 args) (drop 1 argsTypes)) $ mapM inferStmt stmts
      findInEnv (head args) >>= \case
        Nothing -> pure ()
        Just funT -> constraint funT (TVoidFun argsTypes)
      pure (UponD argsTypes name args stmtsT)
    | otherwise -> do
      freshTVars <- mapM (const (TVar <$> fresh)) args
      stmtsT <- pushScope (zip args freshTVars) $ mapM inferStmt stmts
      findInEnv name >>= \case
        Nothing -> pure ()
        Just funT -> constraint funT (TVoidFun freshTVars)
      pure (UponD freshTVars name args stmtsT)

inferStmt :: Statement Parsed -> Infer (Statement Typed)
inferStmt = cata \case
  AssignF i e -> do
    (t, e') <- inferExp e
    findInEnv i >>= \case
      Nothing -> error $ "couldn't find " <> i <> " to assign to"
      Just it -> constraint it t
    pure (Assign i e')

  IfF e thenS elseS -> do
    (t, e') <- inferExp e
    constraint t TBool
    thenS' <- sequence thenS
    elseS' <- sequence elseS
    pure (If e' thenS' elseS')
 
  TriggerF name args
    | map C.toLower name == "send" -> do

      (argTys, args') <- unzip <$> mapM inferExp (drop 1 args)
      findInEnv (case head args of Id n -> n; _ -> undefined) >>= \case
        Nothing   -> -- pure () -- Couldn't find this signal, no problem, we don't know about it but might still exist of course.
                     error $ "Couldn't find " <> show (head args) <> " in Send Trigger"
        Just funT -> constraint funT (TVoidFun (TMessageType:argTys))
      pure (Trigger name $ (case head args of Id n -> Id n; _ -> undefined):args')
    | otherwise -> do
      (argTys, args') <- unzip <$> mapM inferExp args
      findInEnv name >>= \case
        Nothing -> pure () -- If name isn't found it is a notification and don't add any extra constraint
        Just funT -> constraint funT (TVoidFun argTys)
      pure (Trigger name args')

  ForeachF _ name e body -> do
    nt <- TVar <$> fresh
    (t, e') <- inferExp e
    constraint t (TSet nt)
    bodyT <- pushScope [(name, nt)] $ sequence body
    pure (Foreach nt name e' bodyT)


inferExp :: Expr Parsed -> Infer (AType, Expr Typed)
inferExp = cata \case
  IF x -> pure (TInt, I x)
  BF x -> pure (TBool, B x)
  IdF i -> findInEnv i >>= \case
    Nothing -> error $ "Couldn't find id " <> i
    Just t -> pure (t, Id i) 
  InF e1 e2 -> do
    (t1, e1') <- e1
    (t2, e2') <- e2
    constraint t2 (TSet t1)
    pure (TBool, In e1' e2')
    -- TODO: Map
  NotInF e1 e2 -> do
    (t1, e1') <- e1
    (t2, e2') <- e2
    constraint t2 (TSet t1)
    pure (TBool, NotIn e1' e2')
    -- TODO: Map
  EqF e1 e2 -> do
    (t1, e1') <- e1
    (t2, e2') <- e2
    constraint t1 t2
    pure (TBool, Eq e1' e2')
  NotEqF e1 e2 -> do
    (t1, e1') <- e1
    (t2, e2') <- e2
    constraint t1 t2
    pure (TBool, NotEq e1' e2')
  SetF _ s -> do
    s' <- sequence s
    case s' of
      [] -> do
        newTV <- TVar <$> fresh
        pure (TSet newTV, Set newTV [])
      (t, x):xs -> do
        xs' <- forM xs \(t', x') -> do
                constraint t' t
                pure x'
        pure (TSet t, Set t (x:xs'))
  MapF _ m -> error "infer map"
  UnionF _ e1 e2 -> do
    -- TODO: Only works for sets yet, but should also work for maps.
    (t1, e1') <- e1
    (t2, e2') <- e2
    constraint t1 t2
    -- TODO: Constraint to set or map
    pure (t1, Union t1 e1' e2')
  DifferenceF _ e1 e2 -> do
    -- TODO: Only works for sets yet, but should also work for maps.
    (t1, e1') <- e1
    (t2, e2') <- e2
    constraint t1 t2
    -- TODO: Constraint to set or map
    pure (t1, Difference t1 e1' e2')

runInfer :: Infer a -> (a, [Constraint])
runInfer m = evalRWS m mempty (Uniq 0)

solve :: [Constraint] -> Either TypeError Subst
solve constraints = fst <$> runIdentity (runExceptT (runStateT solver (mempty, constraints)))

type Solver = StateT Unifier (ExceptT TypeError Identity)
type Subst = M.Map Int AType
type Unifier = (Subst, [Constraint])
data TypeError = UnificationFail AType AType | UnificationMismatch [AType] [AType] | InfiniteType Int AType deriving Show

solver :: Solver Subst
solver = do
  (su, cs) <- get
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1 <- unifies t1 t2
      put (su1 `compose` su, applySubst su1 cs0)
      solver

unifies :: AType -> AType -> Solver Subst
unifies = \cases
  (TVar v) t -> v `bind` t
  t (TVar v) -> v `bind` t
  (TSet t1) (TSet t2) -> unifies t1 t2
  (TMap t1) (TMap t2) -> unifies t1 t2
  (TVoidFun ts1) (TVoidFun ts2) -> unifyMany ts1 ts2
  t1 t2 -> if t1 == t2 then pure mempty else throwError $ UnificationFail t1 t2

unifyMany :: [AType] -> [AType] -> Solver Subst
unifyMany [] [] = pure mempty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (applySubst su1 ts1) (applySubst su1 ts2)
     pure (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

bind :: Int -> AType -> Solver Subst
bind a t | t == TVar a     = pure mempty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ M.singleton a t
  where
    occursCheck ::  Substitutable a => Int -> a -> Bool
    occursCheck a' t' = a' `S.member` ftv t'

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (applySubst s1) s2 `M.union` s1

class Substitutable a where
  applySubst :: Subst -> a -> a
  ftv :: a -> S.Set Int

instance Substitutable AType where
  applySubst s = cata \case
    TIntF -> TInt
    TBoolF -> TBool
    TStringF -> TString
    TSetF t -> TSet t
    TMapF t -> TMap t
    TVoidFunF ts -> TVoidFun ts
    TClassF n -> TClass n
    TMessageTypeF -> TMessageType
    TVarF i -> M.findWithDefault (TVar i) i s

  ftv = cata \case
    TIntF -> mempty
    TBoolF -> mempty
    TStringF -> mempty
    TSetF t -> t
    TMapF t -> t
    TVoidFunF ts -> mconcat ts
    TClassF _ -> mempty
    TMessageTypeF -> mempty
    TVarF i -> S.singleton i

instance Substitutable a => Substitutable [a] where
  applySubst s as = map (applySubst s) as
  ftv as = mconcat $ map ftv as

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  applySubst s (a, b) = (applySubst s a, applySubst s b)
  ftv (a, b) = ftv a <> ftv b

instance Substitutable (Algorithm Typed) where
  applySubst s (P i st tops) = P i (applySubst s st) (applySubst s tops)
  ftv (P _ st tops) = ftv st <> ftv tops
    
instance Substitutable (StateD Typed) where
  applySubst s (StateD tvs vs) = StateD (applySubst s tvs) vs
  ftv (StateD tvs _) = ftv tvs

instance Substitutable (TopDecl Typed) where
  applySubst s = \case
    UponD tvs i args body -> UponD (applySubst s tvs) i args (applySubst s body)
  ftv = \case
    UponD tvs _ _ body -> ftv tvs <> ftv body

instance Substitutable (Statement Typed) where
  applySubst s = cata \case
    AssignF i e -> Assign i (applySubst s e)
    IfF e thenS elseS -> If (applySubst s e) thenS elseS
    TriggerF i e -> Trigger i (applySubst s e)
    ForeachF tv i e stmts -> Foreach (applySubst s tv) i (applySubst s e) stmts
  ftv = cata \case
    AssignF _ _ -> mempty
    IfF _ thenS elseS -> mconcat (thenS <> elseS)
    TriggerF _ _ -> mempty
    ForeachF tv _ _ stmts -> ftv tv <> mconcat stmts

instance Substitutable (Expr Typed) where
  applySubst s = cata \case
    SetF t n -> Set (applySubst s t) n
    MapF t n -> Map (applySubst s t) n
    UnionF t e f -> Union (applySubst s t) e f
    DifferenceF t e f -> Difference (applySubst s t) e f
    InF e f -> In e f
    NotInF e f -> NotIn e f
    EqF e f -> Eq e f
    NotEqF e f -> NotEq e f
    IF x -> I x
    BF x -> B x
    IdF x -> Id x

  ftv = cata \case
    SetF t e -> ftv t <> mconcat e
    MapF t e -> error "Map"
    UnionF t e f -> ftv t <> e <> f
    DifferenceF t e f -> ftv t <> e <> f
    InF e f -> e <> f
    NotInF e f -> e <> f
    EqF e f -> e <> f
    NotEqF e f -> e <> f
    _ -> mempty

-- Util

fresh :: Infer Int
fresh = do
  Uniq i <- get
  modify' (Uniq . (+1) . coerce)
  pure i

constraint :: AType -> AType -> Infer ()
constraint a b = tell [(a,b)]

pushScope :: [(Identifier, AType)] -> Infer a -> Infer a
pushScope (M.fromList -> l) = local (l:)

-- | find in env with fallback in case there's no such identifier on the env but
-- its still valid since it might be a notification received by other protocols
findInEnv :: Identifier -> Infer (Maybe AType)
findInEnv i' = do
  r <- ask
  pure $ findInEnv' i' r
    where
      findInEnv' :: Identifier -> REnv -> Maybe AType
      findInEnv' i = \case
        [] -> Nothing
        x:xs -> case M.lookup i x of
                  Nothing -> findInEnv' i xs
                  Just t  -> Just t
