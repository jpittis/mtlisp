{-# LANGUAGE OverloadedStrings #-}
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

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Result a = Either (ParseErrorBundle Text Void) a

parseSexpr :: Text -> Result Sexpr
parseSexpr = parse sexpr ""

parseProgr :: Text -> Result [Sexpr]
parseProgr = parse progr ""

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

progr :: Parser [Sexpr]
progr = lexeme (many sexpr)

sexpr :: Parser Sexpr
sexpr =
  lexeme $
        SAtom <$> atom
    <|> try sLambda
    <|> try sDefine
    <|> try sIf
    <|> SList <$> parens (many sexpr)
  where
    sLambda :: Parser Sexpr
    sLambda =
      parens $ do
        lexeme $ string "lambda"
        params <- lexeme $ parens (many . lexeme $ aSymbol)
        body   <- sexpr
        pure $ SLambda params body
    sDefine :: Parser Sexpr
    sDefine =
      parens $ do
        lexeme $ string "define"
        sym <- lexeme aSymbol
        val <- sexpr
        pure $ SDefine sym val
    sIf :: Parser Sexpr
    sIf =
      parens $ do
        lexeme $ string "if"
        test  <- sexpr
        consq <- sexpr
        alt   <- sexpr
        pure $ SIf test consq alt

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
