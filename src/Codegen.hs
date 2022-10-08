{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Codegen where

-- TODO: Really, we should only do this after passing through an initial desugaring phase into another intermediate representation which is then typechecked.
-- Since it doesn't really matter in the long term, I'll just accept this mess

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

import Data.Functor.Foldable
import Data.Bifunctor

import Control.Monad
import Control.Monad.RWS.CPS

import Language.Java.Syntax hiding (Assign, NotEq)
import qualified Language.Java.Syntax as J

import Syntax

type Env = M.Map Identifier ()
type Babel = RWS ([(Scope, [Identifier])], ([Identifier], [Identifier])) () -- Reader: (Scope, Requests, Indications)
                 ([Identifier], ([Identifier], [Identifier])) -- State: (Request Handlers, Notification Subscriptions, Messages watched)

-- | Generate code for a Protocol, given its name and identifier
codegen :: (Identifier, Int) -> Algorithm Typed -> J.CompilationUnit
codegen i = runBabel . translateAlg i

data Scope = UponRequest | UponNotification | UponMessage | Init

pushScope :: (Scope, [Identifier]) -> Babel a -> Babel a
pushScope = local . first . (:)

registerRequestHandler :: Identifier -> Babel ()
registerRequestHandler = modify . first . (:)

subscribeNotification :: Identifier -> Babel ()
subscribeNotification = modify . second . first . (:)

registerMessage :: Identifier -> Babel ()
registerMessage = modify . second . second . (:)


-- | Translate algorithm given protocol identifier and protocol name
translateAlg :: (Identifier, Int) -> Algorithm Typed -> Babel J.CompilationUnit
translateAlg (protoName, protoId) (P (InterfaceD requests indications) (StateD varTypes vars) tops) = do
  varTypes' <- mapM translateType varTypes
  -- local (\r -> foldr (\v -> M.insert v ()) r vars) do -- add vars to env
  local (second (<> (requests, indications))) do

    methodDecls <- forM tops translateTop
    (reqHandlers, (subNotis, subMsgs)) <- get
    let
        protoFieldDecls = [ MemberDecl $ FieldDecl [Public, Final] stringType [VarDecl (VarId (Ident "PROTO_NAME")) (Just $ InitExp $ Lit $ String protoName)]
                          , MemberDecl $ FieldDecl [Public, Final] (PrimType ShortT) [VarDecl (VarId (Ident "PROTO_ID")) (Just $ InitExp $ Lit $ Int $ toInteger protoId)]
                          ] 
        fieldDecls = map (\(v, t) -> MemberDecl $ FieldDecl [Private] t [VarDecl (VarId (Ident v)) Nothing]) (zip vars varTypes')
        constructor = MemberDecl $ ConstructorDecl [Public] [] (Ident protoName) [] [ClassRefType $ ClassType [(Ident "HandlerRegistrationException", [])]] $ ConstructorBody (Just $ SuperInvoke [] [ExpName $ Name $ [Ident "PROTO_NAME"], ExpName $ Name $ [Ident "PROTO_ID"]]) registers
                      -- Requests
        registers   = map (\i -> BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "registerRequestHandler"])   [FieldAccess $ ClassFieldAccess (Name [Ident $ upperFirst i]) (Ident "REQUEST_ID"),      MethodRef (Name [Ident "this"]) (Ident $ "upon" <> upperFirst i)]) reqHandlers

                       -- Notifications
                    <> map (\i -> BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "subscribeNotification"]) [FieldAccess $ ClassFieldAccess (Name [Ident $ upperFirst i]) (Ident "NOTIFICATION_ID"), MethodRef (Name [Ident "this"]) (Ident $ "upon" <> upperFirst i)]) subNotis

                       -- Messages
                       -- TODO: If we only receive messages, then we don't create the channel, and can only set it up after having a channel... how? if we do send then we should create the channel? or just take a placeholder for a channel?
                    -- <> map (\i -> BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "registerMessageSerializer"]) [FieldAccess $ ClassFieldAccess (Name [Ident $ upperFirst i]) (Ident "NOTIFICATION_ID"), MethodRef (Name [Ident "this"]) (Ident $ "upon" <> upperFirst i)]) subMsgs
                    -- <> map (\i -> BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "subscribeNotification"]) [FieldAccess $ ClassFieldAccess (Name [Ident $ upperFirst i]) (Ident "NOTIFICATION_ID"), MethodRef (Name [Ident "this"]) (Ident $ "upon" <> upperFirst i)]) subMsgs
                     
        classBody   = ClassBody $ protoFieldDecls <> fieldDecls <> [constructor] <> methodDecls
    pure $ CompilationUnit Nothing [] [ClassTypeDecl $ ClassDecl [Public] (Ident protoName) [] (Just $ ClassRefType $ ClassType [(Ident "GenericProtocol", [])]) [] classBody]

translateTop :: TopDecl Typed -> Babel Decl
translateTop top = do
  requests <- asks (fst.snd)
  case top of
    UponD argTypes name args stmts -> do
      argTypes' <- mapM translateType argTypes
      case name of
       _| map C.toLower name == "init" -> do
           bodyStmts <- pushScope (Init, args) $ mapM translateStmt stmts
           pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident "init") (map (\(a, t) -> FormalParam [] t False (VarId (Ident a))) (zip args argTypes')) [] Nothing (MethodBody $ Just $ Block bodyStmts)

        | map C.toLower name == "receive" -> do
           case args of
             messageType:from:args' -> do
               bodyStmts <- pushScope (UponMessage, from:args') $ mapM translateStmt stmts
               makeMessage messageType args' (drop 2 argTypes')
               registerMessage messageType
               pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident ("upon" <> upperFirst messageType)) [ FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident $ upperFirst messageType, [])]) False (VarId (Ident "msg"))
                                                                                                              , FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident "Host", [])]) False (VarId (Ident from)) 
                                                                                                              , FormalParam [] (PrimType ShortT) False (VarId (Ident "sourceProto"))
                                                                                                              -- , FormalParam [] (PrimType IntT) False (VarId (Ident "channelId"))
                                                                                                              ] [] Nothing (MethodBody $ Just $ Block bodyStmts)
             _ -> error "Incorrect args for Receive. Excepting Receive(MessageType, src, args...)"

        | name `elem` requests -> do
            registerRequestHandler name
            makeRequest name args argTypes'
            bodyStmts <- pushScope (UponRequest, args) $ mapM translateStmt stmts
            pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident ("upon" <> upperFirst name)) [ FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident $ upperFirst name, [])]) False (VarId (Ident "request"))
                                                                                                    , FormalParam [] (PrimType ShortT) False (VarId (Ident "sourceProto")) ] [] Nothing (MethodBody $ Just $ Block bodyStmts)
        | otherwise -> do
            makeNotification name args argTypes' -- TODO: and then put it in the correct place depending on where its triggered from
            subscribeNotification name
            bodyStmts <- pushScope (UponNotification, args) $ mapM translateStmt stmts
            pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident ("upon" <> upperFirst name)) [ FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident $ upperFirst name, [])]) False (VarId (Ident "notification"))
                                                                                                    , FormalParam [] (PrimType ShortT) False (VarId (Ident "sourceProto")) ] [] Nothing (MethodBody $ Just $ Block bodyStmts)


translateStmt :: Statement Typed -> Babel BlockStmt
translateStmt = fmap BlockStmt . para \case
  AssignF i e -> do
    e' <- translateExp e
    case e of
      -- When we're doing a union, we don't assign the call to add because it returns a boolean
      Union {} -> pure $ ExpStmt e'
      Difference {} -> pure $ ExpStmt e'
      _ -> pure $ ExpStmt $ J.Assign (NameLhs $ Name [Ident i]) EqualA e'

  IfF e (unzip -> (_, thenS)) (unzip -> (_, elseS)) -> do
    e' <- translateExp e
    thenS' <- StmtBlock . Block . map BlockStmt <$> sequence thenS
    case elseS of
      [] ->
        pure $ IfThen e' thenS'
      _  -> do
        elseS' <- StmtBlock . Block . map BlockStmt <$> sequence elseS
        pure $ IfThenElse e' thenS' elseS'

  TriggerF name args -> do
    (_, indications) <- asks snd
    argsExps <- mapM translateExp args
    case name of
     _| name `elem` indications -> do
        -- Is a request on other protocol?
        pure $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "sendRequest"]) [InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst name,[])]) argsExps Nothing, Lit $ String "TODO"]
      | map C.toLower name == "send" -> case zip args argsExps of
          (Id messageType,_):(_, to):(map snd -> argsExps') ->
            pure $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "sendMsg"]) [InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst messageType,[])]) argsExps' Nothing, to]
          _ -> error "impossible :)  can't send without the correct parameters"

      | otherwise -> do
        pure $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "triggerNotification"]) [InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst name,[])]) argsExps Nothing]


    -- makeNotification args argTypes'
    -- pure (ExpStmt $ MethodInv $ MethodCall (Name [Ident i]) args')

  ForeachF t name e (unzip -> (_, body)) -> do
    e' <- translateExp e
    body' <- sequence body
    t' <- translateType t
    pure $ EnhancedFor [] t' (Ident name) e' (StmtBlock . Block . map BlockStmt $ body')


translateExp :: Expr Typed -> Babel Exp
translateExp = para \case
  IF i -> pure $ Lit $ Int i
  BF b -> pure $ Lit $ Boolean b
  IdF i -> do
    i' <- translateIdentifier i
    pure i'
  InF (_, e1) (_, e2) -> do
    e1' <- e1
    e2' <- e2
    pure $ MethodInv $ PrimaryMethodCall e2' [] (Ident "contains") [e1']
  NotInF (_, e1) (_, e2) -> do
    e1' <- e1
    e2' <- e2
    pure $ PreNot $ MethodInv $ PrimaryMethodCall e2' [] (Ident "contains") [e1']
  EqF (p1, e1) (_, e2) -> do
    -- ROMES:TODO:NEXT: == if prim type, (...).equals(...) otherwise
    -- let t = expType p1
    e1' <- e1
    e2' <- e2
    pure $ BinOp e1' Equal e2'
  NotEqF (_, e1) (_, e2) -> do
    e1' <- e1
    e2' <- e2
    pure $ BinOp e1' J.NotEq e2'
  SetF t (unzip -> (_, ss)) -> translateType t >>= \case
    PrimType _ -> error "PrimType (Non-RefType) Set"
    RefType t' -> case ss of
      [] -> pure $ InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident "HashSet", [ActualType t'])]) [] Nothing
      _ -> do
        ss' <- sequence ss
        pure $ InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident "HashSet", [ActualType t'])]) [MethodInv $ TypeMethodCall (Name [Ident "Arrays"]) [] (Ident "asList") ss'] Nothing
  MapF _ m -> error "Map translate"
  UnionF t (_, e1) (p2, e2) -> do
    -- TODO: Only works for sets yet, but should also work for maps.
    e1' <- e1
    case p2 of
      Set _ [x] -> do
        x' <- translateExp x
        pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "add") [x']
      _ -> do
        e2' <- e2
        pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "addAll") [e2']
  DifferenceF t (_, e1) (p2, e2) -> do
    -- TODO: Only works for sets yet, but should also work for maps.
    e1' <- e1
    case p2 of
      Set _ [x] -> do
        x' <- translateExp x
        pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "remove") [x']
      _ -> do
        e2' <- e2
        pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "removeAll") [e2']

translateIdentifier :: Identifier -> Babel Exp
translateIdentifier i = asks fst >>= pure . trId'
  where
    trId' :: [(Scope, [Identifier])] -> Exp
    trId' = \case
      [] -> ExpName $ Name [Ident i]
      (s,l):xs -> case L.find (== i) l of
                    Nothing -> trId' xs
                    Just _  -> case s of
                      UponRequest -> MethodInv $ PrimaryMethodCall (ExpName $ Name [Ident "request"]) [] (Ident $ "get" <> upperFirst i) []
                      UponNotification -> MethodInv $ PrimaryMethodCall (ExpName $ Name [Ident "notification"]) [] (Ident $ "get" <> upperFirst i) []
                      UponMessage -> MethodInv $ PrimaryMethodCall (ExpName $ Name [Ident "msg"]) [] (Ident $ "get" <> upperFirst i) []
                      Init -> ExpName $ Name [Ident i]

translateType :: AType -> Babel Type
translateType = cata \case
  TIntF -> pure $ PrimType IntT
  TBoolF -> pure $ PrimType BooleanT
  TStringF -> pure stringType
  TSetF x -> x >>= \case
    PrimType x' -> error $ show x' <> " is not a boxed/Object type"
    RefType t  -> pure $ RefType $ ClassRefType $ ClassType [(Ident "Set", [ActualType t])]
  TMapF x -> x >>= \case
    PrimType x' -> error $ show x' <> " is not a boxed/Object type"
    RefType t  -> pure $ RefType $ ClassRefType $ ClassType [(Ident "Map", [ActualType t])]
  TVoidFunF _ -> error "Fun type"
  TClassF n -> pure $ RefType $ ClassRefType $ ClassType [(Ident n, [])]
  TMessageTypeF ->
    -- This code is mostly hacked together, this is not good at all, and I know
    -- of multiple many ways of doing this better, but its been hacked from the
    -- start, without a thought, so we'll just wing it, pretend we can generate
    -- MessageTypes into java types, and then never use them
    pure $ RefType $ ClassRefType $ ClassType [(Ident "ThisShouldntEverHappen", [])] -- error "Codegen: MessageType"
  TVarF i -> pure $ RefType $ ClassRefType $ ClassType [(Ident $ "Unknown" <> show i, [])]

makeRequest :: Identifier -> [Identifier] -> [Type] -> Babel ()
makeRequest i ids tys = do
  pure ()

makeNotification :: Identifier -> [Identifier] -> [Type] -> Babel ()
makeNotification i ids tys = do
  pure ()

makeMessage :: Identifier -> [Identifier] -> [Type] -> Babel ()
makeMessage i ids tys = do
  pure ()

stringType :: Type
stringType = RefType $ ClassRefType $ ClassType [(Ident "String", [])]
  
runBabel :: Babel a -> a
runBabel b = fst $ evalRWS b mempty mempty

-- | Upper cases the first letter of a string
upperFirst :: String -> String
upperFirst = \case
  [] -> []
  x:xs -> C.toUpper x:xs
