{-# LANGUAGE OverloadedStrings #-}
-- | Parser Combinators and helpers for parsing s-expressions.
module Parser.Parser
    ( sexpr
    , progr
    , Result
    , parseSexpr
    , parseProgr
    ) where

import Parser.Sexpr

import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Data.Word (Word64)
import Data.Void (Void)
import Data.Scientific (floatingOrInteger)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Either a parser error or an AST.
type Result a = Either (ParseErrorBundle Text Void) a

-- | High level helper to parse an s-expression.
parseSexpr :: Text -> Result Sexpr
parseSexpr = parse sexpr ""

-- | High level helper to parse a series of s-expressions.
parseProgr :: Text -> Result [Sexpr]
parseProgr = parse progr ""

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parser Combinator for a series of s-expressions.
progr :: Parser [Sexpr]
progr = lexeme (many sexpr)

-- | Parser Combinator for an s-expression.
sexpr :: Parser Sexpr
sexpr =
  lexeme $
        SAtom <$> atom
    <|> parens listSexpr
  where
    listSexpr :: Parser Sexpr
    listSexpr =
          sLambda
      <|> sDefine
      <|> sIf
      <|> SList <$> many sexpr
    sLambda :: Parser Sexpr
    sLambda = do
      lexeme $ string "lambda"
      params <- lexeme $ parens (many . lexeme $ aSymbol)
      body   <- sexpr
      pure $ SLambda params body
    sDefine :: Parser Sexpr
    sDefine = do
      lexeme $ string "define"
      sym <- lexeme aSymbol
      val <- sexpr
      pure $ SDefine sym val
    sIf :: Parser Sexpr
    sIf = do
      lexeme $ string "if"
      test  <- sexpr
      consq <- sexpr
      alt   <- sexpr
      pure $ SIf test consq alt

doubleOrInteger :: Parser Atom
doubleOrInteger = do
  num <- L.scientific
  case floatingOrInteger num of
    Left  r -> pure $ ADouble r
    Right i -> pure $ AInteger i

atom :: Parser Atom
atom =
      doubleOrInteger
  <|> ASymbol <$> aSymbol
  <|> AString <$> aString

aSymbol :: Parser Symbol
aSymbol = Symbol . Text.pack <$> (some aSymbolChar)
  where
    aSymbolChar :: Parser Char
    aSymbolChar = alphaNumChar <|> char '-' <|> symbolChar

aString :: Parser Text
aString = quoted textString
  where
    quoted :: Parser a -> Parser a
    quoted = between (char '"') (char '"')

textString :: Parser Text
textString = Text.pack . concat <$> (some stringChar)
  where
    stringChar :: Parser String
    stringChar = escaped <|> allowed
    escaped :: Parser String
    escaped = do
      d <- char '\\'
      c <- oneOf ['\\', '\"']
      pure [d, c]
    allowed :: Parser String
    allowed = do
      c <- noneOf ['\\', '\"']
      pure [c]

parens :: Parser a -> Parser a
parens = between (lexemeChar '(') (char ')')
  where
    lexemeChar :: Char -> Parser Char
    lexemeChar = lexeme . char
