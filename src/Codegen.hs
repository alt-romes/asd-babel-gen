{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Codegen where

-- TODO: Really, we should only do this after passing through an initial desugaring phase into another intermediate representation which is then typechecked.
-- Since it doesn't really matter in the long term, I'll just accept this mess

import Data.String
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

import Data.Functor.Foldable
import Data.Bifunctor

import Control.Monad
import Control.Monad.State
import Control.Monad.RWS.CPS

import Language.Java.Syntax hiding (Assign, NotEq)
import qualified Language.Java.Syntax as J

import Syntax
import Typechecker (expType)

type Env = M.Map Identifier ()
type Babel = RWS ([(Scope, [Identifier])], ([Request], [Indication])) -- Reader: (Scope, (Requests, Indications))
                 W -- Writer: (Messages, Timers)
                 ([Identifier], [Identifier], [Identifier], [Identifier]) -- State: (Request Handlers, Notification Subscriptions, Messages, Timers)

type W = ([(Identifier, [(Identifier, AType)])], [(Identifier, [(Identifier, AType)])])

type Indication = FLDecl
type Request    = FLDecl

codegenProtocols :: [(String, Algorithm Typed)] -- ^ Algorithm and its name
                 -> [(FilePath, J.CompilationUnit)] -- ^ Module name in hierarchy in which the root is @./@ and the associated unit
codegenProtocols protos = first ("./" <>) <$> (`evalState` 100) do

  let (allReqs, allInds) = mconcat $ map (\(_, P (InterfaceD @Typed _ reqs inds) _ _) -> (reqs, inds)) protos

  concat <$> forM protos \(name, p@(P (InterfaceD @Typed ts reqs inds) _ _)) -> do
    let reqTs = zip reqs (fst ts)
        indTs = zip inds (snd ts)
    topI <- get
    let (proto, (messages, timers)) = runBabel $ local (second (<> (allReqs, allInds))) $ translateAlg (upperFirst name, topI) p
    modify' (+1)
    rs <- forM reqTs $ \a@(FLDecl rName _,_) -> do
      i <- freshI
      pure (name <> "/common/requests/" <> upperFirst rName <> ".java", genRequest a i)
    is <- forM indTs $ \a@(FLDecl iName _,_) -> do
      i <- freshI
      pure (name <> "/common/notifications/" <> upperFirst iName <> ".java", genIndication a i)
    ms <- forM messages $ \m@(mName,_) -> do
      i <- freshI
      pure (name <> "/messages/" <> upperFirst mName <> ".java", genMessage m i)
    tms <- forM timers $ \t@(tName,_) -> do
      i <- freshI
      pure (name <> "/timers/" <> upperFirst tName <> ".java", genTimer t i)
    put (topI+100)
    pure ((name <> "/" <> name <> ".java", proto) : (rs <> is <> ms <> tms))

genRequest :: (FLDecl, AType)
           -> Int -- ^ Identifier
           -> J.CompilationUnit
genRequest (FLDecl name (map argName -> args), TFun argTys TVoid) i = genHelperCommon (name, args, argTys) i "REQUEST_ID" "ProtoRequest" []
genRequest _ _ = error "impossible,,, requests should have type (...) -> Void"

genIndication :: (FLDecl, AType)
              -> Int -- ^ Identifier
              -> J.CompilationUnit
genIndication (FLDecl name (map argName -> args), TFun argTys TVoid) i = genHelperCommon (name, args, argTys) i "NOTIFICATION_ID" "ProtoNotification" []
genIndication _ _ = error "impossible,,, indications should have type (...) -> Void"

genMessage :: (Identifier, [(Identifier, AType)]) -> Int -> J.CompilationUnit
genMessage (name, unzip -> (args, argTys)) i = genHelperCommon (name,args,argTys) i "MSG_ID" "ProtoMessage"
                [ MemberDecl $ MethodDecl [Public] [] (Just $ stringType) "toString" [] [] Nothing $
                    MethodBody $ Just $ Block [BlockStmt $ Return $ Just $ Lit $ String $ upperFirst name <> "{}"]
                , MemberDecl $ FieldDecl [Public, Static] (RefType $ ClassRefType $ serializerClassType)
                    [VarDecl (VarId $ "serializer") (Just $ InitExp $ InstanceCreation [] (TypeDeclSpecifier serializerClassType) [] (Just makeSerializerBody))]
                ]
  where
  serializerClassType = ClassType [("ISerializer",[ActualType $ ClassRefType $ ClassType [(Ident $ upperFirst name,[])]])]

  makeSerializerBody :: ClassBody
  makeSerializerBody = ClassBody
                        [ MemberDecl $ MethodDecl [Public] [] Nothing "serialize" [FormalParam [] (classRefType $ upperFirst name) False $ VarId "msg", FormalParam [] (classRefType "ByteBuf") False $ VarId "out"] [ClassRefType $ ClassType [("IOException",[])]] Nothing $
                            MethodBody $ Just $ Block $ concat $
                              zipWith serialT (map Just args) argTys
                        , MemberDecl $ MethodDecl [Public] [] (Just $ classRefType $ upperFirst name) "deserialize" [FormalParam [] (classRefType "ByteBuf") False $ VarId "in"] [ClassRefType $ ClassType [("IOException",[])]] Nothing $
                            MethodBody $ Just $ Block $
                              let (concat -> bls, exps) = unzip (zipWith (deserialT 0) (map Just args) argTys) in
-- TODO: ordNub from list utils something
                              map (\(n,t) -> LocalVars [] t [VarDecl (VarId n) Nothing]) (L.nub $ concatMap neededLocalVars argTys)
                              <> bls
                              <> [BlockStmt $
                                    Return $ Just $ InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst name,[])]) exps Nothing]
                        ]

  neededLocalVars :: AType -> [(Ident,Type)]
  neededLocalVars = \case
    TSet _ -> [("size", PrimType IntT)]
    TClass "UUID" -> [("firstLong", PrimType LongT), ("secondLong", PrimType LongT)]
    TArray _ -> [("size", PrimType IntT)]
    _ -> []


  serialT :: Maybe Identifier -> AType -> [BlockStmt]
  serialT mname = \case
    TSet t -> [ BlockStmt $ ExpStmt $ MethodInv $ PrimaryMethodCall "out" [] "writeInt" [MethodInv $ PrimaryMethodCall nameExp [] "size" []]
              , BlockStmt $ EnhancedFor [] (translateType t) "x" nameExp (StmtBlock $ Block $ serialT Nothing t)
              ]
    TClass "Host" -> [ BlockStmt $ ExpStmt $ MethodInv $ TypeMethodCall (Name ["Host","serializer"]) [] "serialize" [nameExp, "out"]
                     ]

    TClass "UUID" -> [ BlockStmt $ ExpStmt $ MethodInv $ PrimaryMethodCall "out" [] "writeLong" [MethodInv $ PrimaryMethodCall nameExp [] "getMostSignificantBits" []]
                     , BlockStmt $ ExpStmt $ MethodInv $ PrimaryMethodCall "out" [] "writeLong" [MethodInv $ PrimaryMethodCall nameExp [] "getLeastSignificantBits" []]
                     ]

    TArray TByte -> [ BlockStmt $ ExpStmt $ MethodInv $ PrimaryMethodCall "out" [] "writeInt" [FieldAccess $ PrimaryFieldAccess nameExp "length"]
                    , BlockStmt $ IfThen (BinOp (FieldAccess (PrimaryFieldAccess nameExp "length")) GThan (Lit $ Int 0)) $ StmtBlock $ Block $
                        [BlockStmt $ ExpStmt $ MethodInv $ PrimaryMethodCall "out" [] "writeBytes" [nameExp]]
                    ]

    TClass ('U':'n':'k':'n':'o':'w':'n':_) -> error "Can't serialize unknown variables!"

    t -> error $ "Don't know how to serialize " <> show mname <> " " <> show t

    where
      nameExp :: Exp
      nameExp = case mname of
                  Just name -> MethodInv $ PrimaryMethodCall "msg" [] (Ident $ "get" <> upperFirst name) []
                  Nothing   -> "x"

  deserialT :: Int -> Maybe Identifier -> AType -> ([BlockStmt], Exp)
  deserialT it mname = \case
    TSet t -> let (bs, recE) = deserialT (it+1) Nothing t in
      ([ BlockStmt $ ExpStmt $ J.Assign (NameLhs "size") EqualA $ MethodInv $ PrimaryMethodCall "in" [] "readInt" []
       , LocalVars [] (translateType (TSet t)) [VarDecl (VarId $ Ident name) (Just $ InitExp $ InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident "HashSet", [ActualType $ let RefType r = translateType t in r])]) [ExpName "size", Lit $ Int 1] Nothing)]
       , BlockStmt $ BasicFor (Just $ ForLocalVars [] (PrimType IntT) [VarDecl (VarId $ Ident [alphalist !! it]) (Just $ InitExp $ Lit $ Int 0)])
                             (Just $ BinOp (fromString [alphalist !! it]) LThan "size")
                             (Just $ [PostIncrement (fromString [alphalist !! it])])
                             (StmtBlock $ Block $ bs <> [BlockStmt $ ExpStmt $ MethodInv $ PrimaryMethodCall (fromString name) [] "add" [recE]])
      ], fromString name)
    TClass "Host" ->
      let ep = MethodInv $ TypeMethodCall (Name ["Host","serializer"]) [] "deserialize" ["in"] in
      case mname of
        Nothing -> ([],ep)
        Just n' -> ([LocalVars [] (translateType (TClass "Host")) [VarDecl (VarId $ fromString name) $ Just $ InitExp ep]], fromString n')

    TClass "UUID" ->
      let common = [ BlockStmt $ ExpStmt $ J.Assign (NameLhs "firstLong") EqualA $ MethodInv $ PrimaryMethodCall "in" [] "readLong" []
                   , BlockStmt $ ExpStmt $ J.Assign (NameLhs "secondLong") EqualA $ MethodInv $ PrimaryMethodCall "in" [] "readLong" []
                   ]
          exp = InstanceCreation [] (TypeDeclSpecifier $ ClassType [("UUID",[])]) ["firstLong", "secondLong"] Nothing
       in case mname of
        Nothing -> (common, exp)
        Just n' -> (common <> [LocalVars [] (translateType (TClass "UUID")) [VarDecl (VarId $ fromString n') $ Just $ InitExp exp]], fromString n')

    TArray TByte ->
      let
       in case mname of
            Nothing -> error "how to recurse something that has a byte[]?"
            Just n' ->
              ([ BlockStmt $ ExpStmt $ J.Assign (NameLhs "size") EqualA $ MethodInv $ PrimaryMethodCall "in" [] "readInt" []
               , LocalVars [] (PrimType ByteT) [VarDecl (VarId $ fromString n') $ Just $ InitExp $ ArrayCreate (PrimType ByteT) ["size"] 0]
               , BlockStmt $ IfThen (BinOp "size" GThan (Lit $ Int 0)) $ ExpStmt $ MethodInv $ PrimaryMethodCall "in" [] "readBytes" [fromString n']
               ], fromString n')

    TClass ('U':'n':'k':'n':'o':'w':'n':_) -> error "Can't serialize unknown variables!"

    t -> error $ "Don't know how to serialize " <> show mname <> " " <> show t
    where
      name = case mname of Nothing -> error "deserialize shouldn't try to use a name when it doesn't need one"; Just n -> n
      alphalist = ['a'..]



genTimer :: (Identifier, [(Identifier, AType)]) -> Int -> J.CompilationUnit
genTimer (name, unzip -> (args, argTys)) i = genHelperCommon (name, args, argTys) i "TIMER_ID" "ProtoTimer"
                [ MemberDecl $ MethodDecl [Public] [] (Just $ classRefType "ProtoTimer") (Ident "clone") [] [] Nothing $ MethodBody $ Just $ Block [BlockStmt $ Return $ Just This]
                ]

genHelperCommon :: (Identifier, [Identifier], [AType])
                -> Int -- ^ Numeric Identifier
                -> String -- ^ Name of static identifier field
                -> String -- ^ Name of class it should extend
                -> [Decl] -- ^ Extra declarations in the class body
                -> J.CompilationUnit
genHelperCommon (name, args, argTys) nid sif protoExtends extraBody = do
    let
        argTys' = map translateType argTys
        protoFieldDecls = [ MemberDecl $ FieldDecl [Public, Static, Final] (PrimType ShortT) [VarDecl (VarId (Ident sif)) (Just $ InitExp $ Lit $ Int $ toInteger nid)]
                          ] 
        fieldDecls = zipWith (\v t -> MemberDecl $ FieldDecl [Private, Final] t [VarDecl (VarId (Ident v)) Nothing]) args argTys'
        constructor = MemberDecl $ ConstructorDecl [Public] [] (Ident $ upperFirst name) (zipWith (\x t -> FormalParam [] t False (VarId (Ident x))) args argTys') [] $ ConstructorBody (Just $ SuperInvoke [] [ExpName $ Name [Ident sif]]) registers
        registers = map (\x -> BlockStmt $ ExpStmt $ J.Assign (FieldLhs $ PrimaryFieldAccess This (Ident x)) EqualA (ExpName $ Name [Ident x])) args
        methodDecls = zipWith (\x t -> MemberDecl $ MethodDecl [Public] [] (Just t) (Ident ("get" <> upperFirst x)) [] [] Nothing (MethodBody $ Just $ Block [BlockStmt $ Return $ Just $ ExpName $ Name [Ident x]])) args argTys'
        classBody   = ClassBody $ protoFieldDecls <> fieldDecls <> [constructor] <> methodDecls <> extraBody
     in
        CompilationUnit Nothing [ImportDecl False (Name [Ident "java", Ident "util", Ident "*"]) False, ImportDecl False (Name [Ident "pt", Ident "unl", Ident "fct", Ident "di", Ident "novasys", Ident "babel", Ident "*"]) False]
                                [ClassTypeDecl $ ClassDecl [Public] (Ident $ upperFirst name) [] (Just $ ClassRefType $ ClassType [(Ident protoExtends, [])]) [] classBody]


  -- let (requests, indications) = bimap concat concat $ unzip $ map ((\(InterfaceD () reqs inds) -> (reqs, inds)) . interfaceD) ps

  -- reqVars <- forM requests $
  --   \(r, args) -> (r,) . TVoidFun <$> mapM (const (TVar <$> fresh)) args

  -- indVars <- forM indications $
  --   \(i, args) -> (i,) . TVoidFun <$> mapM (const (TVar <$> fresh)) args

-- Should use codegenProtocols
-- codegen :: (Identifier, Int) -> Algorithm Typed -> J.CompilationUnit
-- codegen i = runBabel . translateAlg i

data Scope = UponRequest | UponNotification | UponMessage | Init | Procedure | UponTimer

pushScope :: (Scope, [Identifier]) -> Babel a -> Babel a
pushScope = local . first . (:)

registerRequestHandler :: Identifier -> Babel ()
registerRequestHandler x = modify (\(r,n,m,t) -> (x:r,n,m,t))

subscribeNotification :: Identifier -> Babel ()
subscribeNotification x = modify (\(r,n,m,t) -> (r,x:n,m,t))

registerMessage :: Identifier -> Babel ()
registerMessage x = modify (\(r,n,m,t) -> (r,n,x:m,t))

registerTimer :: Identifier -> Babel ()
registerTimer x = modify (\(r,n,m,t) -> (r,n,m,x:t))

-- | Translate algorithm given protocol identifier and protocol name
translateAlg :: (Identifier, Int) -> Algorithm Typed -> Babel J.CompilationUnit
translateAlg (protoName, protoId) (P (InterfaceD _ _ _) (StateD varTypes vars) tops) = do
  methodDecls <- forM tops translateTop
  (reqHandlers, subNotis, subMsgs, subTimers) <- get
  let
      varTypes' = map translateType varTypes
      protoFieldDecls = [ MemberDecl $ FieldDecl [Public, Static, Final] stringType [VarDecl (VarId (Ident "PROTO_NAME")) (Just $ InitExp $ Lit $ String protoName)]
                        , MemberDecl $ FieldDecl [Public, Static, Final] (PrimType ShortT) [VarDecl (VarId (Ident "PROTO_ID")) (Just $ InitExp $ Lit $ Int $ toInteger protoId)]
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

                     -- Timers
                  <> map (\i -> BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "registerTimerHandler"]) [FieldAccess $ ClassFieldAccess (Name [Ident $ upperFirst i]) (Ident "TIMER_ID"), MethodRef (Name [Ident "this"]) (Ident $ "upon" <> upperFirst i)]) subTimers
                   
      classBody   = ClassBody $ protoFieldDecls <> fieldDecls <> [constructor] <> methodDecls
  pure $ CompilationUnit Nothing [ImportDecl False (Name [Ident "java", Ident "util", Ident "*"]) False, ImportDecl False (Name [Ident "pt", Ident "unl", Ident "fct", Ident "di", Ident "novasys", Ident "babel", Ident "*"]) False]
                                 [ClassTypeDecl $ ClassDecl [Public] (Ident protoName) [] (Just $ ClassRefType $ ClassType [(Ident "GenericProtocol", [])]) [] classBody]

translateTop :: TopDecl Typed -> Babel Decl
translateTop top = do
  (requests, indications) <- asks (bimap (fmap (\(FLDecl n _) -> n)) (fmap (\(FLDecl n _) -> n)) . snd)
  case top of
    UponReceiveD argTypes messageType (map argName -> args) stmts -> do
      case args of
        from:args' -> do
          bodyStmts <- pushScope (UponMessage, from:args') $ mapM translateStmt stmts
          makeMessage messageType args' (drop 1 argTypes)
          registerMessage messageType
          pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident ("upon" <> upperFirst messageType)) [ FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident $ upperFirst messageType, [])]) False (VarId (Ident "msg"))
                                                                                                         , FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident "Host", [])]) False (VarId (Ident from)) 
                                                                                                         , FormalParam [] (PrimType ShortT) False (VarId (Ident "sourceProto"))
                                                                                                         -- , FormalParam [] (PrimType IntT) False (VarId (Ident "channelId"))
                                                                                                         ] [] Nothing (MethodBody $ Just $ Block bodyStmts)
        _ -> error "Incorrect args for Receive. Excepting Receive(MessageType, src, args...)"

    ProcedureD argTypes (FLDecl name (map argName -> args)) stmts -> do
      let argTypes' = map translateType argTypes
      bodyStmts <- pushScope (Procedure, args) $ mapM translateStmt stmts
      pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident name) (map (\(a, t) -> FormalParam [] t False (VarId (Ident a))) (zip args argTypes')) [] Nothing (MethodBody $ Just $ Block bodyStmts)

    UponTimerD argTypes (FLDecl name (map argName -> args)) stmts -> do
      bodyStmts <- pushScope (UponTimer, args) $ mapM translateStmt stmts
      registerTimer name
      makeTimer name args argTypes
      pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident ("upon" <> upperFirst name)) [ FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident $ upperFirst name, [])]) False (VarId (Ident "timer"))
                                                                                              , FormalParam [] (PrimType ShortT) False (VarId (Ident "timerId")) ] [] Nothing (MethodBody $ Just $ Block bodyStmts)

    UponD argTypes (FLDecl name (map argName -> args)) stmts -> do
      let argTypes' = map translateType argTypes
      case name of

       _| map C.toLower name == "init" -> do
           bodyStmts <- pushScope (Init, args) $ mapM translateStmt stmts
           pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident "init") (map (\(a, t) -> FormalParam [] t False (VarId (Ident a))) (zip args argTypes')) [] Nothing (MethodBody $ Just $ Block bodyStmts)

        | name `elem` requests -> do
            bodyStmts <- pushScope (UponRequest, args) $ mapM translateStmt stmts
            registerRequestHandler name
            pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident ("upon" <> upperFirst name)) [ FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident $ upperFirst name, [])]) False (VarId (Ident "request"))
                                                                                                    , FormalParam [] (PrimType ShortT) False (VarId (Ident "sourceProto")) ] [] Nothing (MethodBody $ Just $ Block bodyStmts)
        | name `elem` indications -> do
            bodyStmts <- pushScope (UponNotification, args) $ mapM translateStmt stmts
            subscribeNotification name
            pure $ MemberDecl $ MethodDecl [Private] [] Nothing (Ident ("upon" <> upperFirst name)) [ FormalParam [] (RefType $ ClassRefType $ ClassType [(Ident $ upperFirst name, [])]) False (VarId (Ident "notification"))
                                                                                                    , FormalParam [] (PrimType ShortT) False (VarId (Ident "sourceProto")) ] [] Nothing (MethodBody $ Just $ Block bodyStmts)
        | otherwise -> error $ "Unknown upon event " <> show name


translateStmt :: Statement Typed -> Babel BlockStmt
translateStmt = para \case
  ExprStatementF e -> BlockStmt . ExpStmt <$> translateExp e
  AssignF mt i e -> do
    e' <- translateExp e
    case mt of
      Nothing -> case e of
        -- When we're doing a union, we don't assign the call to add because it returns a boolean
        Union {} -> pure $ BlockStmt $ ExpStmt e'
        Difference {} -> pure $ BlockStmt $ ExpStmt e'
        _ -> pure $ BlockStmt $ ExpStmt $ J.Assign (NameLhs $ Name [Ident i]) EqualA e'
      Just t -> case e of
        Union {} -> error "undefined union assignment for new local variables"
        Difference {} -> error "undefined difference assignment for new local variables"
        _ -> pure $ LocalVars [] (translateType t) [VarDecl (VarId $ Ident i) (Just $ InitExp e')]

  IfF e (unzip -> (_, thenS)) (unzip -> (_, elseS)) -> do
    e' <- translateExp e
    thenS' <- StmtBlock . Block <$> sequence thenS
    case elseS of
      [] ->
        pure $ BlockStmt $ IfThen e' thenS'
      _  -> do
        elseS' <- StmtBlock . Block <$> sequence elseS
        pure $ BlockStmt $ IfThenElse e' thenS' elseS'

  TriggerSendF messageType args -> do
    argsExps <- mapM translateExp args
    case zip args argsExps of
        (_, to):(map snd -> argsExps') ->
          pure $ BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "sendMsg"]) [InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst messageType,[])]) argsExps' Nothing, to]
        _ -> error "impossible :)  can't send without the correct parameters"

  SetupPeriodicTimerF name timer args -> do
    timerExp <- translateExp timer
    argsExps <- mapM translateExp args
    pure $ BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "setupPeriodicTimer"]) [InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst name,[])]) argsExps Nothing, timerExp, timerExp]

  SetupTimerF name timer args -> do
    timerExp <- translateExp timer
    argsExps <- mapM translateExp args
    pure $ BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "setupTimer"]) [InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst name,[])]) argsExps Nothing, BinOp timerExp Mult (Lit (Int 1000))]

  TriggerF (FLCall name args) -> do
    (requests, indications) <- asks (bimap (map (\(FLDecl n _) -> n)) (map (\(FLDecl n _) -> n)) . snd)
    argsExps <- mapM translateExp args
    case name of
     _| name `elem` requests -> do
        -- Is a request on other protocol?
        pure $ BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "sendRequest"])
                [ InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst name,[])]) argsExps Nothing
                , Lit $ String "TODO"]

      | name `elem` indications -> do
        pure $ BlockStmt $ ExpStmt $ MethodInv $ MethodCall (Name [Ident "triggerNotification"]) [InstanceCreation [] (TypeDeclSpecifier $ ClassType [(Ident $ upperFirst name,[])]) argsExps Nothing]

      | otherwise -> error $ "Unknown trigger " <> name


    -- makeNotification args argTypes'
    -- pure (ExpStmt $ MethodInv $ MethodCall (Name [Ident i]) args')

  ForeachF t name e (unzip -> (_, body)) -> do
    e' <- translateExp e
    body' <- sequence body
    let t' = translateType t
    pure $ BlockStmt $ EnhancedFor [] t' (Ident name) e' (StmtBlock . Block $ body')


translateExp :: Expr Typed -> Babel Exp
translateExp = para \case
  IF i -> pure $ Lit $ Int i
  BF b -> pure $ Lit $ Boolean b
  BottomF -> pure $ Lit Null
  IdF _ i -> translateIdentifier i
  InF (_, e1) (_, e2) -> do
    e1' <- e1
    e2' <- e2
    pure $ MethodInv $ PrimaryMethodCall e2' [] (Ident "contains") [e1']
  NotInF (_, e1) (_, e2) -> do
    e1' <- e1
    e2' <- e2
    pure $ PreNot $ MethodInv $ PrimaryMethodCall e2' [] (Ident "contains") [e1']
  BOpF bop (p1, e1) (p2, e2) -> do
    e1' <- e1
    e2' <- e2
    case bop of
      Syntax.EQ ->
        case expType p1 of
          TInt  -> pure $ BinOp e1' Equal e2'
          TBool -> pure $ BinOp e1' Equal e2'
          TNull -> pure $ BinOp e1' Equal e2'
          _     -> case expType p2 of
                     TNull -> pure $ BinOp e1' Equal e2'
                     _     -> pure $ MethodInv $ PrimaryMethodCall e1' [] (Ident "equals") [e2']
      Syntax.NE ->
        case expType p1 of
          TInt  -> pure $ BinOp e1' J.NotEq e2'
          TBool -> pure $ BinOp e1' J.NotEq e2'
          TNull -> pure $ BinOp e1' J.NotEq e2'
          _     -> case expType p2 of
                     TNull -> pure $ BinOp e1' J.NotEq e2'
                     _     -> pure $ PreNot $ MethodInv $ PrimaryMethodCall e1' [] (Ident "equals") [e2']
      Syntax.LE -> pure $ BinOp e1' LThanE e2'
      Syntax.GE -> pure $ BinOp e1' GThanE e2'
      Syntax.LT -> pure $ BinOp e1' LThan e2'
      Syntax.GT -> pure $ BinOp e1' GThan e2'
      Syntax.AND -> pure $ BinOp e1' And e2'
      Syntax.OR  -> pure $ BinOp e1' Or e2'
  SetF t (unzip -> (_, ss)) -> case translateType t of
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
  SizeOfF (_, e) -> do
    e' <- e
    pure $ MethodInv $ PrimaryMethodCall e' [] (Ident "size") []

  CallF _ (FLCall name args) -> do
    argsExps <- mapM translateExp args
    pure $ MethodInv $ MethodCall (Name [Ident name]) argsExps


translateIdentifier :: Identifier -> Babel Exp
translateIdentifier i = asks fst >>= pure . trId'
  where
    trId' :: [(Scope, [Identifier])] -> Exp
    trId' = \case
      [] -> ExpName $ Name [Ident i]
      (UponMessage, from:_):_
        | i == from -> ExpName $ Name [Ident i] -- Is Host "from"
      (s,l):xs -> case L.find (== i) l of
                    Nothing -> trId' xs
                    Just _  -> case s of
                      UponRequest -> MethodInv $ PrimaryMethodCall (ExpName $ Name [Ident "request"]) [] (Ident $ "get" <> upperFirst i) []
                      UponNotification -> MethodInv $ PrimaryMethodCall (ExpName $ Name [Ident "notification"]) [] (Ident $ "get" <> upperFirst i) []
                      UponMessage -> MethodInv $ PrimaryMethodCall (ExpName $ Name [Ident "msg"]) [] (Ident $ "get" <> upperFirst i) []
                      Init -> ExpName $ Name [Ident i]
                      Procedure -> ExpName $ Name [Ident i]
                      UponTimer -> MethodInv $ PrimaryMethodCall (ExpName $ Name [Ident "timer"]) [] (Ident $ "get" <> upperFirst i) []

translateType :: AType -> Type
translateType = cata \case
  TVoidF -> error "void type"
  TNullF -> RefType $ ClassRefType $ ClassType []
  TIntF -> PrimType IntT
  TByteF -> PrimType ByteT
  TArrayF t -> RefType $ ArrayType t
  TBoolF -> PrimType BooleanT
  TStringF -> stringType
  TSetF x -> case x of
    PrimType x' -> error $ show x' <> " is not a boxed/Object type"
    RefType t  -> RefType $ ClassRefType $ ClassType [(Ident "Set", [ActualType t])]
  TMapF x -> case x of
    PrimType x' -> error $ show x' <> " is not a boxed/Object type"
    RefType t  -> RefType $ ClassRefType $ ClassType [(Ident "Map", [ActualType t])]
  TFunF _ _ -> error "Fun type"
  TClassF n -> RefType $ ClassRefType $ ClassType [(Ident n, [])]
  TVarF i -> RefType $ ClassRefType $ ClassType [(Ident $ "Unknown" <> show i, [])]

classRefType :: Identifier -> Type
classRefType i = RefType $ ClassRefType $ ClassType [(Ident i, [])]

makeMessage :: Identifier -> [Identifier] -> [AType] -> Babel ()
makeMessage i ids tys = tell ([(i, zip ids tys)], mempty)

makeTimer :: Identifier -> [Identifier] -> [AType] -> Babel ()
makeTimer i ids tys = tell (mempty, [(i, zip ids tys)])

stringType :: Type
stringType = RefType $ ClassRefType $ ClassType [(Ident "String", [])]

runBabel :: Babel a -> (a, W)
runBabel b = evalRWS b mempty mempty

-- | Upper cases the first letter of a string
upperFirst :: String -> String
upperFirst = \case
  [] -> []
  x:xs -> C.toUpper x:xs

freshI :: State Int Int
freshI = do
  i <- get
  put (let !x = i+1 in x)
  pure i

instance IsString Ident where
  fromString = Ident

instance IsString Name where
  fromString x = Name [Ident x]

instance IsString Exp where
  fromString x = ExpName $ Name [Ident x]
