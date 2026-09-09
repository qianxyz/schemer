module Main (main) where

import Schemer
import Test.Hspec
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = hspec $ do
  describe "parseExpr" $ do
    it "parses a string with an escaped quote" $ do
      parse parseExpr "" "\"a\\\"b\"" `shouldBe` Right (String "a\"b")
