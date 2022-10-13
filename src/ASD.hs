{-# LANGUAGE BlockArguments #-}
module ASD where

-- import Parser
-- import Typechecker
-- import Codegen

-- import Text.Megaparsec

-- import qualified Language.Java.Pretty as J

-- testBabelProto :: FilePath -> IO ()
-- testBabelProto fp = do
--   alg <- parse pAlg fp <$> readFile fp
--   case alg of
--     Left e -> print e
--     Right p -> case typecheckProtocols [p] of
--         Left e -> print e
--         Right [tp] -> print $ J.pretty $ codegen (fp, 100) tp
--         _ -> undefined
