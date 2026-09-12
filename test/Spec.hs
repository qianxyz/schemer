module Main (main) where

import Data.Complex (Complex ((:+)))
import Data.Either (isLeft)
import Data.Ratio ((%))
import Schemer
import Test.Hspec
import Text.ParserCombinators.Parsec (ParseError, parse)

parseSExp :: String -> Either ParseError SExp
parseSExp = parse parseExpr ""

main :: IO ()
main = hspec $ do
  describe "atoms" $ do
    it "parses an identifier" $
      parseSExp "abc" `shouldBe` Right (Atom "abc")
    it "parses a bare i as an atom" $
      parseSExp "i" `shouldBe` Right (Atom "i")
    describe "peculiar identifiers" $ do
      it "parses + as an atom" $
        parseSExp "+" `shouldBe` Right (Atom "+")
      it "parses - as an atom" $
        parseSExp "-" `shouldBe` Right (Atom "-")

  describe "strings" $ do
    it "parses a simple string" $
      parseSExp "\"abc\"" `shouldBe` Right (String "abc")
    it "parses a string with an escaped quote" $
      parseSExp "\"a\\\"b\"" `shouldBe` Right (String "a\"b")
    it "parses a string with an escaped backslash" $
      parseSExp "\"a\\\\b\"" `shouldBe` Right (String "a\\b")
    it "rejects an unterminated string" $
      parseSExp "\"abc" `shouldSatisfy` isLeft

  describe "booleans" $ do
    it "parses #t" $
      parseSExp "#t" `shouldBe` Right (Bool True)
    it "parses #f" $
      parseSExp "#f" `shouldBe` Right (Bool False)

  describe "characters" $ do
    it "parses a lowercase character" $
      parseSExp "#\\a" `shouldBe` Right (Char 'a')
    it "parses an uppercase character" $
      parseSExp "#\\A" `shouldBe` Right (Char 'A')
    it "parses a named character" $
      parseSExp "#\\space" `shouldBe` Right (Char ' ')
    it "parses a named character with mixed case" $
      parseSExp "#\\Space" `shouldBe` Right (Char ' ')
    it "rejects an unknown character name" $
      parseSExp "#\\ab" `shouldSatisfy` isLeft

  describe "numbers" $ do
    describe "integers" $ do
      it "parses a decimal" $
        parseSExp "123" `shouldBe` Right (Real 123)
      it "parses a #d decimal" $
        parseSExp "#d123" `shouldBe` Right (Real 123)
      it "parses a binary" $
        parseSExp "#b1010" `shouldBe` Right (Real 10)
      it "parses an octal" $
        parseSExp "#o12" `shouldBe` Right (Real 10)
      it "parses a hexadecimal" $
        parseSExp "#xA" `shouldBe` Right (Real 10)
      it "parses a positive sign" $
        parseSExp "+1" `shouldBe` Right (Real 1)
      it "parses a negative sign" $
        parseSExp "-1" `shouldBe` Right (Real (-1))

    describe "floats" $ do
      it "parses a decimal float" $
        parseSExp "1.5" `shouldBe` Right (Real $ Float 1.5)
      it "parses a #d decimal float" $
        parseSExp "#d1.5" `shouldBe` Right (Real $ Float 1.5)
      it "parses a positive float" $
        parseSExp "+1.5" `shouldBe` Right (Real $ Float 1.5)
      it "parses a negative float" $
        parseSExp "-1.5" `shouldBe` Right (Real $ Float (-1.5))
      it "rejects a float with no integer part" $
        parseSExp ".5" `shouldSatisfy` isLeft
      it "rejects a float with no fractional part" $
        parseSExp "1." `shouldSatisfy` isLeft

    describe "rationals" $ do
      it "parses a rational" $
        parseSExp "1/2" `shouldBe` Right (Real $ Rational (1 % 2))
      it "parses a negative rational" $
        parseSExp "-1/2" `shouldBe` Right (Real $ Rational ((-1) % 2))
      it "parses a hex rational" $
        parseSExp "#x1/A" `shouldBe` Right (Real $ Rational (1 % 10))
      it "parses a binary rational" $
        parseSExp "#b101/11" `shouldBe` Right (Real $ Rational (5 % 3))
      it "normalises a rational with denominator 1 to an integer" $
        parseSExp "4/2" `shouldBe` Right (Real 2)
      it "rejects a zero denominator" $
        parseSExp "1/0" `shouldSatisfy` isLeft

    describe "complex numbers" $ do
      it "parses real plus imaginary" $
        parseSExp "1+2i" `shouldBe` Right (Complex (1 :+ 2))
      it "parses real minus imaginary" $
        parseSExp "1-2i" `shouldBe` Right (Complex (1 :+ (-2)))
      it "parses real plus unit imaginary" $
        parseSExp "1+i" `shouldBe` Right (Complex (1 :+ 1))
      it "parses real minus unit imaginary" $
        parseSExp "1-i" `shouldBe` Right (Complex (1 :+ (-1)))
      it "parses +i" $
        parseSExp "+i" `shouldBe` Right (Complex (0 :+ 1))
      it "parses -i" $
        parseSExp "-i" `shouldBe` Right (Complex (0 :+ (-1)))
      it "parses a pure imaginary" $
        parseSExp "+2i" `shouldBe` Right (Complex (0 :+ 2))
      it "parses a negative pure imaginary" $
        parseSExp "-2i" `shouldBe` Right (Complex (0 :+ (-2)))
      it "parses float components" $
        parseSExp "1.5+2.5i" `shouldBe` Right (Complex (Float 1.5 :+ Float 2.5))
      it "parses rational components" $
        parseSExp "1/2+1/3i" `shouldBe` Right (Complex (Rational (1 % 2) :+ Rational (1 % 3)))
      it "parses mixed negative components" $
        parseSExp "-1/2-1.5i" `shouldBe` Right (Complex (Rational ((-1) % 2) :+ Float (-1.5)))
      it "parses a hex complex" $
        parseSExp "#x1+Ai" `shouldBe` Right (Complex (1 :+ 10))
      it "parses a binary complex" $
        parseSExp "#b1+1i" `shouldBe` Right (Complex (1 :+ 1))
      it "rejects a zero denominator in the real part" $
        parseSExp "1/0+i" `shouldSatisfy` isLeft
      it "rejects a sign with no imaginary unit" $
        parseSExp "1+2" `shouldSatisfy` isLeft
      it "rejects a trailing sign" $
        parseSExp "1+" `shouldSatisfy` isLeft
