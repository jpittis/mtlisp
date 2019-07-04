{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Parser.Sexpr
import Parser.Parser

import Test.Hspec

spec :: Spec
spec =
  describe "Parser.Parser" $
    it "parses atoms" $ do
      parseSexpr "123.3"      `shouldBe` atom ADouble 123.3
      parseSexpr "1234"       `shouldBe` atom AInteger 1234
      parseSexpr "foobar"     `shouldBe` atom ASymbol (Symbol "foobar")
      parseSexpr "\"foobar\"" `shouldBe` atom AString "foobar"

atom :: (a -> Atom) -> a -> Result Sexpr
atom constructor = Right . SAtom . constructor
