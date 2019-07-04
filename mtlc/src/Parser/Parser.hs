{-# LANGUAGE OverloadedStrings #-}
module Parser.Parser
    ( sexpr
    , Result
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

aSymbol :: Parser Symbol
aSymbol = Symbol . Text.pack <$> (some symbolChar)
  where
    symbolChar :: Parser Char
    symbolChar = alphaNumChar <|> char '-' <|> symbolChar