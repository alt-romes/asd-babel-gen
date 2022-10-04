{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
module ASD where

import Prelude
import Data.Coerce
import Control.Monad.Reader

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Data.Dynamic

import qualified Language.Java.Syntax as J

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

newtype Program = P [TopDecl]
  deriving Show

data TopDecl
 = StateD [Identifier]
 | UponD Identifier [Identifier] [Statement]
  deriving Show
 -- | InterfaceD [RequestD] [IndicationD]
 -- | ProcedureD Expr
 -- | MessagesD  --
 -- | TimersD    --

data Statement
  = Assign Identifier Expr
  | If Expr [Statement] [Statement]
  | Trigger Identifier [Expr]
  | TriggerSend Identifier Expr [Expr] -- ^ Trigger Send(MessageType, host, args...) (TriggerSend MessageType host [arg])
  | Foreach Identifier Expr [Statement]
  deriving Show

data Expr
  = I Int
  | B Bool
  | Set [Either (Expr, Expr) Expr] -- maps vs sets; {(m,p)} vs {m}
  | Id Identifier
  | In Expr Expr
  | NotIn Expr Expr
  | Union Expr Expr
  | Difference Expr Expr
  | Eq Expr Expr
  | NotEq Expr Expr
  deriving Show



testProg :: Program
testProg = P
  [ StateD ["myself", "neighbours", "received", "channelReady"]
  , UponD "Init" ["self"] [ Assign "myself" (Id "self")
                          , Assign "neighbours" (Set [])
                          , Assign "received" (Set [])
                          , Assign "channelReady" (B False)
                          ]
  , UponD "ChannelCreated" [] [ Assign "channelReady" (B True) ]
  , UponD "broadcastRequest" ["mid", "s", "m"] [ If (Id "channelReady") [Trigger "floodMessage" [Id "mid", Id "s", Id "m", Id "myself"]] [] ]
  , UponD "floodMessage" ["mid", "s", "m", "from"] [ If (Id "mid" `NotIn` Id "received") [ Assign "received" (Id "received" `Union` Set [Right $ Id "mid"])
                                                                                         -- , Trigger "Notify" (undefined)
                                                                                         , Foreach "host" (Id "neighbours") [If (Id "host" `NotEq` Id "from") [TriggerSend "FloodMessage" (Id "host") [Id "mid", Id "s"]] []]
                                                                                         ] [] ]
  , UponD "neighbourUp" ["upNeighbours"] [Foreach "h" (Id "upNeighbours") [Assign "neighbours" (Id "neighbours" `Union` Set [Right $ Id "h"])]]
  , UponD "neighbourDown" ["downNeighbours"] [Foreach "h" (Id "downNeighbours") [Assign "neighbours" (Id "neighbours" `Difference` Set [Right $ Id "h"])]]
  ]




-- type ASDM = Reader ()

-- type Java = [CompilationUnit]


-- translate :: Program -> Java
-- translate = map translateTopDecl . coerce

-- translateTopDecl :: TopDecl -> ASDM Java
-- translateTopDecl = cata $ \case
  
