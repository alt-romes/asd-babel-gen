{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Maybe
import Data.Void
import Data.Functor
import Data.Bifunctor
import qualified Data.Char as C
-- import qualified Data.ByteString as BS


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Syntax

type Parser = Parsec Void String

parseProtocol :: String -- ^ Filename
              -> String -- ^ String to parse
              -> Either String (Algorithm Parsed)
parseProtocol x = first errorBundlePretty . parse pAlg x

pAlg :: Parser (Algorithm Parsed)
pAlg = nonIndented $ P <$> pInterfaceD <*> pStateD <*> many pTopDecl

pInterfaceD :: Parser (InterfaceD Parsed)
pInterfaceD = indentBlock do
  _ <- symbol' "interface" *> symbol ":"
  pure $ L.IndentMany Nothing (\(bimap mconcat mconcat . unzip -> (reqs, indications)) ->
                                  pure $ InterfaceD () reqs indications) do
    reqs <- optional $ indentBlock do
      _ <- symbol' "requests" *> symbol ":"
      pure $ L.IndentMany Nothing pure ((,) <$> identifier <*> parens args)
    indications <- optional $ indentBlock do
      _ <- symbol' "indications" *> symbol ":"
      pure $ L.IndentMany Nothing pure ((,) <$> identifier <*> parens args)
    pure (fromMaybe [] reqs, fromMaybe [] indications) 

pStateD :: Parser (StateD Parsed)
pStateD = indentBlock do
  _ <- symbol' "state" *> symbol ":"
  pure $ L.IndentMany Nothing (pure . StateD ()) identifier

pTopDecl :: Parser (TopDecl Parsed)
pTopDecl = choice
  [ uponD
  ]

  where
    uponD = indentBlock do
      _ <- symbol' "upon"
      i <- identifier
      ids <- parens args
      _ <- symbol "do" <* optional (symbol ":")
      pure $ L.IndentMany Nothing (pure . UponD @Parsed () i ids) pStatement

pStatement :: Parser (Statement Parsed)
pStatement = choice
  [ pIf
  , symbol' "trigger" $> Trigger <*> identifier <*> parens (many (pExp <* optional (symbol ",")))
  , pForeach
  , Assign <$> identifier <* symbol "<-" <*> pExp
  ]
  where
    pIf :: Parser (Statement Parsed)
    pIf = do
      (cond, thenS) <- indentBlock do
        cond <- symbol' "if" *> pExp <* symbol' "then" <* optional (symbol ":")
        pure $ L.IndentMany Nothing (pure . (cond,)) pStatement
      (fromMaybe [] -> elseS) <- optional $ indentBlock do
        _ <- symbol' "else"
        pure $ L.IndentMany Nothing pure pStatement
      pure (If cond thenS elseS)

    pForeach :: Parser (Statement Parsed)
    pForeach = indentBlock do
      (name, iterable) <- symbol' "foreach" $> (,) <*> identifier <* (symbol "∈" <|> symbol' "in") <*> pExp <* symbol "do" <* optional (symbol ":")
      pure $ L.IndentMany Nothing (pure . Foreach () name iterable) pStatement


pExp :: Parser (Expr Parsed)
pExp = makeExprParser
  (choice [ I <$> integer
          , B <$> (symbol' "true" $> True <|> symbol' "false" $> False)
          , Bottom <$  (symbol "⊥" <|> symbol' "null")
          , Set () <$> (symbol' "{" *> (many pExp <* optional (symbol ",")) <* symbol' "}")
          -- , Map () <$> TODO
          , Id () <$> identifier
          ])
  [ [ binary "U" (Union ())
    , binary "\\" (Difference ())
    ]
  , [ InfixL (In <$ (symbol "∈" <|> symbol' "in"))
    , InfixL (NotIn <$ (symbol "∉" <|> (symbol' "not" <* symbol' "in")))
    ]
  , [ binary "=" Eq
    , binary "/=" NotEq ]
  ]


binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

args :: Parser [Arg]
args = many ((Arg <$> identifier <*> optional (symbol ":" *> atype)) <* optional (symbol ","))

atype :: Parser AType
atype = choice
  [ TClass <$> identifier
  ]

--- Lexing

sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "//") empty

scn :: Parser ()
scn = L.space
  space1
  (L.skipLineComment "//")
  empty

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symbol' :: String -> Parser String
symbol' = L.symbol' sc

integer :: Parser Integer
integer = lexeme L.decimal

identifier :: Parser String
identifier = lexeme (takeWhile1P (Just "alpha num identifier") C.isAlphaNum)
