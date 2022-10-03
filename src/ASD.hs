{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
module ASD where

import Prelude

import Data.Dynamic
import Control.Monad.Free
import Control.Monad.Free.TH

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


data ASTF f where
  -- State :: [Dynamic] -> f -> ASTF f
  Upon  :: Eq a => String -> a -> ASTF ()

makeFree ''ASTF

type AST = Free ASTF

interface = undefined
requests = undefined
indications = undefined
state = undefined

(<--) = undefined
(u) = undefined
(∉) = undefined
(+++) = undefined

upon = undefined
trigger = undefined

data State =
  Delivered

-- example1 : AST ()
example1 = do

  -- interface --
  --   requests --
  --     []
  --   indications --
  --     []

  upon "init" do
    Delivered <-- []

  upon "pp2pSend" \[p, m] -> do
    trigger "sp2pSend" [p, m]

  upon "sp2pDeliver" \[s, m] -> do
    if m ∉ Delivered then do
       Delivered <-- Delivered +++ m
       trigger "pp2pDeliver" [s, m]

    else pure ()

