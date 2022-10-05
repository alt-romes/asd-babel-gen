{-# LANGUAGE TemplateHaskell #-}
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
deriving instance Show (XStateD p) => Show (StateD p)

data TopDecl p
 = UponD (XUponD p) Identifier [Identifier] [Statement p]
 -- | InterfaceD [RequestD] [IndicationD]
 -- | ProcedureD Expr
 -- | MessagesD  --
 -- | TimersD    --
deriving instance (Show (XUponD p), Show (XForeach p)) => Show (TopDecl p)

data Statement p
  = Assign Identifier Expr
  | If Expr [Statement p] [Statement p]
  | Trigger Identifier [Expr]
  | TriggerSend Identifier Expr [Expr] -- ^ Trigger Send(MessageType, host, args...) (TriggerSend MessageType host [arg])
  | Foreach (XForeach p) Identifier Expr [Statement p]
deriving instance Show (XForeach p) => Show (Statement p)

data Expr
  = I Integer
  | B Bool
  | Set [Expr] -- sets; {m}
  | Map [(Expr, Expr)] -- maps; {(m,p)}
  | Id Identifier
  | In Expr Expr
  | NotIn Expr Expr
  | Union Expr Expr
  | Difference Expr Expr
  | Eq Expr Expr
  | NotEq Expr Expr
  deriving Show

data AType
  = TInt
  | TBool
  | TSet AType
  | TMap AType
  | TUnknown
  | TVoidFun [AType]
  | TVar Int
  deriving Eq

data Parsed
data Typed


-- Trees that Grow
type family XStateD  p
type family XUponD   p
type family XForeach p

type instance XStateD  Parsed = ()
type instance XStateD  Typed  = [AType]
type instance XUponD   Parsed = ()
type instance XUponD   Typed  = [AType]
type instance XForeach Parsed = ()
type instance XForeach Typed  = AType


makeBaseFunctor ''Statement
makeBaseFunctor ''Expr
makeBaseFunctor ''AType


