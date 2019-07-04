{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ParserSpec (spec) where

import Parser.Sexpr
import Parser.Parser

import Test.Hspec

import Data.Word (Word64)
import Data.Text (Text)
import Data.Void (Void)
import System.Directory (doesFileExist)
import qualified Text.Read as Text (read)
import Test.HUnit.Base (assertFailure)
import Text.Megaparsec (ParseErrorBundle)
import qualified Data.Text.IO as Text (readFile)

spec :: Spec
spec =
  describe "Parser.Parser" $ do
    it "parses atoms" $ do
      parseSexpr "123.3"      `shouldBe` (Right . SAtom . ADouble  $ 123.3)
      parseSexpr "1234"       `shouldBe` (Right . SAtom . AInteger $ 1234)
      parseSexpr "foobar"     `shouldBe` (Right . SAtom . ASymbol  $ Symbol "foobar")
      parseSexpr "\"foobar\"" `shouldBe` (Right . SAtom . AString  $ "foobar")

    it "parses lists" $ do
      parseSexpr "()"           `shouldBe` (Right . SList $ [])
      parseSexpr "(1 2 3)"      `shouldBe` (Right . SList $ [int 1, int 2, int 3])
      parseSexpr "(1 (2 ()) 3)" `shouldBe` (Right . SList $ [int 1, SList [int 2, SList []], int 3])

    it "parses lambdas" $
      parseSexpr "(lambda (x y) (add x y))" `shouldBe`
        (Right $ SLambda [Symbol "x", Symbol "y"] (SList [sym "add", sym "x", sym "y"]))

    it "parses define" $
      parseSexpr "(define x 5)" `shouldBe` (Right $ SDefine (Symbol "x") (int 5))

    it "parses if" $
      parseSexpr "(if x y z)" `shouldBe` (Right $ SIf (sym "x") (sym "y") (sym "z"))

    it "parses fib" $ do
      compareGoldenFile "test/input/fib.mtl" "test/output/fib" >>= assertGoldenResult

int :: Word64 -> Sexpr
int = SAtom . AInteger

sym :: Text -> Sexpr
sym = SAtom . ASymbol . Symbol

assertGoldenResult :: GoldenResult -> IO ()
assertGoldenResult result =
  case result of
    ParseError err -> assertFailure $ show err
    NotEqual _ _   -> assertFailure "NotEqual"
    _ -> pure ()

type ParseError = ParseErrorBundle Text Void

data GoldenResult =
    ParseError ParseError
  | NotEqual [Sexpr] [Sexpr]
  | Equal
  | Created

compareGoldenFile :: FilePath -> FilePath -> IO GoldenResult
compareGoldenFile inputPath outputPath = do
  input <- Text.readFile inputPath
  case parseProgr input of
    Left  err      -> pure $ ParseError err
    Right expected ->
      readFileIfExists outputPath >>= \case
        Nothing -> do
          writeFile outputPath (show expected)
          pure Created
        Just output ->
          let found = read output in
          if expected == found then
            pure Equal
          else
            pure $ NotEqual expected found
  where
    readFileIfExists :: FilePath -> IO (Maybe String)
    readFileIfExists path = doesFileExist path >>= \case
      True  -> Just <$> readFile outputPath
      False -> pure Nothing
