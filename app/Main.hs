module Main where

import Syntax
import Typechecker
import Codegen

import Language.Java.Pretty

testProg :: Algorithm Parsed
testProg = P
  (InterfaceD ["broadcastRequest"] [])
  (StateD () ["myself", "neighbours", "received", "channelReady"])
  [ UponD () "Init" ["self"] [ Assign "myself" (Id "self")
                          , Assign "neighbours" (Set () [])
                          , Assign "received" (Set () [])
                          , Assign "channelReady" (B False)
                          ]
  , UponD () "ChannelCreated" [] [ Assign "channelReady" (B True) ]
  , UponD () "broadcastRequest" ["mid", "s", "m"] [ If (Id "channelReady") [Trigger "Send" [Id "FloodMessage", Id "myself", Id "mid", Id "s", Id "m"]] [] ] -- TODO: Should be a CALL
  , UponD () "Receive" ["FloodMessage", "from", "mid", "s", "m"] [ If (Id "mid" `NotIn` Id "received") [ Assign "received" (Union () (Id "received") (Set () [Id "mid"]))
                                                                                         , Trigger "deliverNotification" [Id "mid", Id "s", Id "m"]
                                                                                         , Foreach () "host" (Id "neighbours") [If (Id "host" `NotEq` Id "from") [Trigger "Send" [Id "FloodMessage", Id "host", Id "mid", Id "s", Id "m"]] []]
                                                                                         ] [] ]
  , UponD () "neighbourUp" ["upNeighbours"] [Foreach () "h" (Id "upNeighbours") [Assign "neighbours" (Union () (Id "neighbours") (Set () [Id "h"]))]]
  , UponD () "neighbourDown" ["downNeighbours"] [Foreach () "h" (Id "downNeighbours") [Assign "neighbours" (Difference () (Id "neighbours") (Set () [Id "h"]))]]
  ]


main :: IO ()
main = do
  case codegen ("FloodBroadcast", 200) <$> typecheck testProg of
    Right p -> print $ pretty p
    Left e  -> print e
