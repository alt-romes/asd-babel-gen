module Main where

import Syntax

testProg :: Algorithm Parsed
testProg = P
  (StateD () ["myself", "neighbours", "received", "channelReady"])
  [ UponD () "Init" ["self"] [ Assign "myself" (Id "self")
                          , Assign "neighbours" (Set () [])
                          , Assign "received" (Set () [])
                          , Assign "channelReady" (B False)
                          ]
  , UponD () "ChannelCreated" [] [ Assign "channelReady" (B True) ]
  , UponD () "broadcastRequest" ["mid", "s", "m"] [ If (Id "channelReady") [Trigger "floodMessage" [Id "mid", Id "s", Id "m", Id "myself"]] [] ]
  , UponD () "floodMessage" ["mid", "s", "m", "from"] [ If (Id "mid" `NotIn` Id "received") [ Assign "received" (Union () (Id "received") (Set () [Id "mid"]))
                                                                                         -- , Trigger "Notify" (undefined)
                                                                                         , Foreach () "host" (Id "neighbours") [If (Id "host" `NotEq` Id "from") [TriggerSend "FloodMessage" (Id "host") [Id "mid", Id "s"]] []]
                                                                                         ] [] ]
  , UponD () "neighbourUp" ["upNeighbours"] [Foreach () "h" (Id "upNeighbours") [Assign "neighbours" (Union () (Id "neighbours") (Set () [Id "h"]))]]
  , UponD () "neighbourDown" ["downNeighbours"] [Foreach () "h" (Id "downNeighbours") [Assign "neighbours" (Difference () (Id "neighbours") (Set () [Id "h"]))]]
  ]


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
