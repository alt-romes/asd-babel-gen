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

data Algorithm p = P (StateD p) [TopDecl p]

data StateD p = StateD (XStateD p) [Identifier]

data TopDecl p
 = UponD (XUponD p) Identifier [Identifier] [Statement p]
 -- | InterfaceD [RequestD] [IndicationD]
 -- | ProcedureD Expr
 -- | MessagesD  --
 -- | TimersD    --

data Statement p
  = Assign Identifier (Expr p)
  | If (Expr p) [Statement p] [Statement p]
  | Trigger Identifier [Expr p]
  | TriggerSend Identifier (Expr p) [Expr p] -- ^ Trigger Send(MessageType, host, args...) (TriggerSend MessageType host [arg])
  | Foreach (XForeach p) Identifier (Expr p) [Statement p]

data Expr p
  = I Integer
  | B Bool
  | Set (XSet p) [Expr p] -- sets; {m}
  | Map (XMap p) [(Expr p, Expr p)] -- maps; {(m,p)}
  | Id Identifier
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
  | TUnknown
  | TVoidFun [AType]
  | TVar Int
  deriving (Show, Eq)

data Parsed
data Typed

-- Trees that Grow
type family XStateD  p
type family XUponD   p
type family XForeach p
type family XSet p
type family XMap p
type family XUnion p
type family XDifference p

type instance XStateD  Parsed = ()
type instance XStateD  Typed  = [AType]
type instance XUponD   Parsed = ()
type instance XUponD   Typed  = [AType]
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

makeBaseFunctor ''Statement
makeBaseFunctor ''Expr
makeBaseFunctor ''AType

