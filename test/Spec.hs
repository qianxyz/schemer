module Main (main) where

import Data.Either (isLeft)
import Schemer
import Test.Hspec
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = hspec $ do
  describe "parseExpr" $ do
    it "parses a simple string" $ do
      parse parseExpr "" "\"abc\"" `shouldBe` Right (String "abc")
    it "parses a string with an escaped quote" $ do
      parse parseExpr "" "\"a\\\"b\"" `shouldBe` Right (String "a\"b")
    it "parses a string with an escaped backslash" $ do
      parse parseExpr "" "\"a\\\\b\"" `shouldBe` Right (String "a\\b")
    it "rejects a unterminated string" $ do
      parse parseExpr "" "\"abc" `shouldSatisfy` isLeft

    it "parses a boolean true" $ do
      parse parseExpr "" "#t" `shouldBe` Right (Bool True)
    it "parses a boolean false" $ do
      parse parseExpr "" "#f" `shouldBe` Right (Bool False)

    it "parses a decimal number" $ do
      parse parseExpr "" "123" `shouldBe` Right (Number 123)
    it "parses a binary number" $ do
      parse parseExpr "" "#b1010" `shouldBe` Right (Number 10)
    it "parses an octal number" $ do
      parse parseExpr "" "#o12" `shouldBe` Right (Number 10)
    it "parses a #d decimal number" $ do
      parse parseExpr "" "#d123" `shouldBe` Right (Number 123)
    it "parses a hexadecimal number" $ do
      parse parseExpr "" "#xA" `shouldBe` Right (Number 10)

    it "parses a lowercase character literal" $ do
      parse parseExpr "" "#\\a" `shouldBe` Right (Char 'a')
    it "parses an uppercase character literal" $ do
      parse parseExpr "" "#\\A" `shouldBe` Right (Char 'A')
    it "parses a named character literal" $ do
      parse parseExpr "" "#\\space" `shouldBe` Right (Char ' ')
    it "parses a named character literal with mixed case" $ do
      parse parseExpr "" "#\\Space" `shouldBe` Right (Char ' ')
    it "rejects an unknown character name" $ do
      parse parseExpr "" "#\\ab" `shouldSatisfy` isLeft

    it "parses a decimal float" $ do
      parse parseExpr "" "1.5" `shouldBe` Right (Float 1.5)
    it "parses a #d decimal float" $ do
      parse parseExpr "" "#d1.5" `shouldBe` Right (Float 1.5)
    it "rejects a float with no integer part" $ do
      parse parseExpr "" ".5" `shouldSatisfy` isLeft
    it "rejects a float with no fractional part" $ do
      parse parseExpr "" "1." `shouldSatisfy` isLeft
