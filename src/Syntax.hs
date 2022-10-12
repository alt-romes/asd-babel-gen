{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
module Syntax where

import Data.Functor.Foldable.TH

{-
Interface:
    Requests:
        request(arg1, ..., argn)
    Indications:
        indication(arg1, ..., argn)

State:
  var 1
  var 2

Handlers: // Are atomic
  Upon Init(arg1, ..., argn) do ... done
  Upon event_name(arg1, ..., argn) do done

Procedure:
  Procedure name(arg1, ..., argn) do
  Call procedureName(arg1, ..., argn)

Messages:
  Trigger send(MessageName, dest (which is a pid), arg1, ..., argn)
  Upon Receive(MessageName, source, arg1, ..., argn)

Timers:
  Setup [Periodic] Timer DoStuff(Δt, arg1, ..., argn)
  Upon Timer DoStuff(arg1, ..., argn)
  Cancel [Periodic] Timer TimerName

-}

type Identifier = String

data Algorithm p = P { interfaceD :: InterfaceD p
                     , stateD     :: StateD p
                     , topDecls   :: [TopDecl p] }

data InterfaceD p = InterfaceD (XInterfaceD p) [FLDecl] [FLDecl] -- [Request] [Indication], only the method name matters

-- Function Like Declaration
data FLDecl = FLDecl Identifier [Arg] deriving Show
-- Function Like Call
data FLCall p = FLCall Identifier [Expr p]

data StateD p = StateD (XStateD p) [Identifier]

data Arg = Arg { argName :: Identifier, argType :: Maybe AType }
  deriving Show

data TopDecl p
 = UponD (XUponD p) FLDecl [Statement p]
 | UponReceiveD (XUponReceiveD p) Identifier [Arg] [Statement p]
 -- | ProcedureD Expr
 -- | MessagesD  --
 -- | TimersD    --

data Statement p
  = Assign Identifier (Expr p)
  | If (Expr p) [Statement p] [Statement p]
  | TriggerSend Identifier [Expr p]
  | Trigger (FLCall p)
  | Foreach (XForeach p) Identifier (Expr p) [Statement p]

data Expr p
  = I Integer
  | B Bool
  | Bottom
  | Set (XSet p) [Expr p] -- sets; {m}
  | Map (XMap p) [(Expr p, Expr p)] -- maps; {(m,p)}
  | Id (XId p) Identifier
  | In (Expr p) (Expr p)
  | NotIn (Expr p) (Expr p)
  | Union (XUnion p) (Expr p) (Expr p)
  | Difference (XDifference p) (Expr p) (Expr p)
  | Eq (Expr p) (Expr p)
  | NotEq (Expr p) (Expr p)

data AType
  = TInt
  | TBool
  | TString
  | TSet AType
  | TMap AType
  | TVoidFun [AType]
  | TVar Int
  | TClass Identifier
  | TNull
  deriving (Show, Eq)

data Parsed
data Typed

-- Trees that Grow
type family XStateD  p
type family XUponD   p
type family XUponReceiveD   p
type family XForeach p
type family XSet p
type family XMap p
type family XUnion p
type family XDifference p
type family XId p
type family XInterfaceD p

type instance XStateD  Parsed = ()
type instance XStateD  Typed  = [AType]
type instance XUponD   Parsed = ()
type instance XUponD   Typed  = [AType]
type instance XUponReceiveD Parsed = ()
type instance XUponReceiveD Typed = [AType]
type instance XForeach Parsed = ()
type instance XForeach Typed  = AType
type instance XSet Parsed = ()
type instance XSet Typed = AType
type instance XMap Parsed = ()
type instance XMap Typed = AType
type instance XUnion Parsed = ()
type instance XUnion Typed = AType
type instance XDifference Parsed = ()
type instance XDifference Typed = AType
type instance XId Parsed = ()
type instance XId Typed = AType
type instance XInterfaceD Parsed = ()
type instance XInterfaceD Typed = ([AType], [AType])

deriving instance Show (Expr Parsed)
deriving instance Show (Expr Typed)
deriving instance Show (Statement Parsed)
deriving instance Show (Statement Typed)
deriving instance Show (StateD Parsed)
deriving instance Show (StateD Typed)
deriving instance Show (TopDecl Parsed)
deriving instance Show (TopDecl Typed)
deriving instance Show (Algorithm Parsed)
deriving instance Show (Algorithm Typed)
deriving instance Show (InterfaceD Parsed)
deriving instance Show (InterfaceD Typed)
deriving instance Show (FLCall Parsed)
deriving instance Show (FLCall Typed)

makeBaseFunctor ''Statement
makeBaseFunctor ''Expr
makeBaseFunctor ''AType
