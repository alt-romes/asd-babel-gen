{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Function
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding

import Control.Category ((>>>))

import Control.Exception
import System.Environment
import System.OsPath
import System.Directory.OsPath
import qualified System.File.OsPath as F

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
  getArgs >>= \case
    [] -> putStrLn "Usage: asd <PSEUDO_CODE_DIRECTORY_PATH>"
    path':_ -> do
      path <- encodeUtf path'
      protocolFiles <- listDirectory path

      let protocolNames = dropExtension <$> protocolFiles
      print protocolNames
      protocolsContents <- mapM (fmap decodeUtf8Lenient . F.readFile') ((path </>) <$> protocolFiles)
      mapM T.putStrLn protocolsContents
      -- map (parseProtocol >>> either (throwIO . show) pure) protocolsContents

      -- typedProtocols <- typecheckProtocols protocols    & either (throwIO . show) pure
      -- babelTower     <- codegenProtocols (zip protocolNames typedProtocols) & either (throwIO . show) pure

      case codegen ("FloodBroadcast", 200) <$> typecheck testProg of
        Right p -> print $ pretty p
        Left e  -> print e
