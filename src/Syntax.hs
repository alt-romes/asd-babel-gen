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
 | ProcedureD (XProcedureD p) FLDecl [Statement p]
 | UponTimerD (XUponTimerD p) FLDecl [Statement p]

data Statement p
  = Assign (XAssign p) (ALhs p) (Expr p)
  | If (Expr p) [Statement p] [Statement p]
  | TriggerSend Identifier [Expr p] -- TODO: Expr p "from" out of list of args
  | Trigger (FLCall p)
  | SetupPeriodicTimer Identifier (Expr p {- period -}) [Expr p]
  | SetupTimer Identifier (Expr p {- period -}) [Expr p]
  | CancelTimer (FLCall p)
  | Foreach (XForeach p) Pat (Expr p) [Statement p]
  | ExprStatement (Expr p)

data ALhs p
  = IdA Identifier
  | MapA Identifier (Expr p)

data Pat
  = IdP Identifier
  | TupleP Identifier Identifier
  deriving Show

data Expr p
  = I Integer
  | B Bool
  | Bottom
  | Tuple (Expr p) (Expr p)
  | SetOrMap (XSetOrMap p) [Expr p]
  | Id (XId p) Identifier
  | SizeOf (Expr p)                   -- e.g. #received
  | BOp BOp (Expr p) (Expr p)
  | Call (XCall p) (FLCall p)
  | MapAccess (XMapAccess p) Identifier (Expr p)

-- data UOp
--   =

data BOp
  = EQ
  | NE
  | GE
  | LE
  | LT
  | GT
  | AND
  | OR
  | ADD
  | MINUS
  | MUL
  | DIV
  | SUBSETEQ
  | IN
  | NOTIN
  | UNION
  | DIFFERENCE
  deriving Show

data AType
  = TVoid
  | TInt
  | TBool
  | TString
  | TSet AType
  | TMap AType AType
  | TFun [AType] AType
  | TVar Int
  | TClass Identifier
  | TNull
  | TByte
  | TArray AType
  | TTuple AType AType
  deriving (Show, Eq)

data Parsed
data Typed

-- Trees that Grow
type family XStateD  p
type family XUponD   p
type family XUponReceiveD p
type family XUponTimerD p
type family XProcedureD  p
type family XForeach p
type family XSetOrMap p
type family XUnion p
type family XDifference p
type family XId p
type family XInterfaceD p
type family XCall p
type family XAssign p
type family XSubsetOf  p
type family XMapAccess  p

type instance XMapAccess  Parsed = ()
type instance XMapAccess  Typed  = AType
type instance XAssign  Parsed = ()
type instance XAssign  Typed  = Maybe AType -- Just Type if is a new local variable of type Type
type instance XCall  Parsed = ()
type instance XCall  Typed  = AType
type instance XStateD  Parsed = ()
type instance XStateD  Typed  = [AType]
type instance XUponD   Parsed = ()
type instance XUponD   Typed  = [AType]
type instance XUponTimerD Parsed = ()
type instance XUponTimerD Typed = [AType]
type instance XUponReceiveD Parsed = ()
type instance XUponReceiveD Typed = [AType]
type instance XProcedureD Parsed = ()
type instance XProcedureD Typed = [AType]
type instance XForeach Parsed = ()
type instance XForeach Typed  = AType
type instance XSetOrMap Parsed = ()
type instance XSetOrMap Typed = AType
type instance XSubsetOf Parsed = ()
type instance XSubsetOf Typed = AType
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
deriving instance Show (ALhs Parsed)
deriving instance Show (ALhs Typed)

makeBaseFunctor ''Statement
makeBaseFunctor ''Expr
makeBaseFunctor ''AType
