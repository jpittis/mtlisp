{-# LANGUAGE OverloadedStrings #-}
module Parser.Parser
    ( sexpr
    , Result
    , parseSexpr
    ) where

import Parser.Sexpr

import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Data.Word (Word64)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

type Result a = Either (ParseErrorBundle Text Void) a

sexpr :: Parser Sexpr
sexpr = lexeme (SAtom <$> atom)

-- TODO: Replace with scientific and Data.Scientific
doubleOrInteger :: Parser Atom
doubleOrInteger =
      ADouble  <$> try L.float
  <|> AInteger <$> L.decimal

atom :: Parser Atom
atom =
      doubleOrInteger
  <|> ASymbol <$> aSymbol
  <|> AString <$> aString

aSymbol :: Parser Symbol
aSymbol = Symbol . Text.pack <$> (some symbolChar)
  where
    symbolChar :: Parser Char
    symbolChar = alphaNumChar <|> char '-'

parseSexpr :: Text -> Result Sexpr
parseSexpr = parse sexpr ""

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
