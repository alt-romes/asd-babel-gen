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

pFLDecl :: Parser FLDecl
pFLDecl = FLDecl <$> identifier <*> parens args

pFLCall :: Parser (FLCall Parsed)
pFLCall = FLCall <$> identifier <*> parens argsExp

pInterfaceD :: Parser (InterfaceD Parsed)
pInterfaceD = indentBlock do
  _ <- symbol' "interface" *> symbol ":"
  pure $ L.IndentMany Nothing (\(bimap mconcat mconcat . unzip -> (reqs, indications)) ->
                                  pure $ InterfaceD () reqs indications) do
    reqs <- optional $ indentBlock do
      _ <- symbol' "requests" *> symbol ":"
      pure $ L.IndentMany Nothing pure pFLDecl
    indications <- optional $ indentBlock do
      _ <- symbol' "indications" *> symbol ":"
      pure $ L.IndentMany Nothing pure pFLDecl
    pure (fromMaybe [] reqs, fromMaybe [] indications) 

pStateD :: Parser (StateD Parsed)
pStateD = indentBlock do
  _ <- symbol' "state" *> symbol ":"
  pure $ L.IndentMany Nothing (pure . StateD ()) identifier

pTopDecl :: Parser (TopDecl Parsed)
pTopDecl = choice
  [ uponReceiveD
  , uponD
  , procedureD
  ]

  where
    procedureD = indentBlock do
      fld <- symbol' "procedure" *> pFLDecl <* symbol "do" <* optional (symbol ":")
      pure $ L.IndentMany Nothing (pure . ProcedureD @Parsed () fld) pStatement

    uponReceiveD = indentBlock do
      (i,as) <- try (symbol' "upon" *> symbol' "receive") *> parens ((,) <$> identifier <* symbol "," <*> args) <* symbol "do" <* optional (symbol ":")
      pure $ L.IndentMany Nothing (pure . UponReceiveD @Parsed () i as) pStatement

    uponD = indentBlock do
      fld <- symbol' "upon" *> pFLDecl <* symbol "do" <* optional (symbol ":")
      pure $ L.IndentMany Nothing (pure . UponD @Parsed () fld) pStatement

pStatement :: Parser (Statement Parsed)
pStatement = choice
  [ pIf
  , pForeach
  , symbol' "call" $> Call <*> pFLCall
  , try (symbol' "trigger" *> symbol' "send") $> uncurry TriggerSend <*> parens ((,) <$> identifier <* symbol "," <*> argsExp)
  , symbol' "trigger" $> Trigger <*> pFLCall
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

argsExp :: Parser [Expr Parsed]
argsExp = many (pExp <* optional (symbol ","))

atype :: Parser AType
atype = makeExprParser
          (choice [TClass <$> identifier])
          [[Postfix $ TArray <$ symbol "[" <* symbol "]"]]

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
