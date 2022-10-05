module Main where

import Syntax

testProg :: Algorithm
testProg = P
  (StateD ["myself", "neighbours", "received", "channelReady"])
  [ UponD "Init" ["self"] [ Assign "myself" (Id "self")
                          , Assign "neighbours" (Set [])
                          , Assign "received" (Set [])
                          , Assign "channelReady" (B False)
                          ]
  , UponD "ChannelCreated" [] [ Assign "channelReady" (B True) ]
  , UponD "broadcastRequest" ["mid", "s", "m"] [ If (Id "channelReady") [Trigger "floodMessage" [Id "mid", Id "s", Id "m", Id "myself"]] [] ]
  , UponD "floodMessage" ["mid", "s", "m", "from"] [ If (Id "mid" `NotIn` Id "received") [ Assign "received" (Id "received" `Union` Set [Id "mid"])
                                                                                         -- , Trigger "Notify" (undefined)
                                                                                         , Foreach "host" (Id "neighbours") [If (Id "host" `NotEq` Id "from") [TriggerSend "FloodMessage" (Id "host") [Id "mid", Id "s"]] []]
                                                                                         ] [] ]
  , UponD "neighbourUp" ["upNeighbours"] [Foreach "h" (Id "upNeighbours") [Assign "neighbours" (Id "neighbours" `Union` Set [Id "h"])]]
  , UponD "neighbourDown" ["downNeighbours"] [Foreach "h" (Id "downNeighbours") [Assign "neighbours" (Id "neighbours" `Difference` Set [Id "h"])]]
  ]


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
