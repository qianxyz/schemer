{-# LANGUAGE OverloadedStrings #-}

module Schemer.DisplaySpec (spec) where

import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))
import Data.Text (Text, pack)
import Data.Text.Display (display)
import Data.Vector (fromList)
import Schemer
import Test.Hspec
import Text.ParserCombinators.Parsec (parse)

-- | Parse a string, then display the result again.
roundTrip :: String -> Either String Text
roundTrip input = case parse parseExpr "" input of
  Left err -> Left (show err)
  Right val -> Right (display val)

spec :: Spec
spec = do
  describe "atoms" $ do
    it "displays an identifier" $
      display (Atom "list->vector") `shouldBe` "list->vector"

  describe "booleans" $ do
    it "displays true" $
      display (Bool True) `shouldBe` "#t"
    it "displays false" $
      display (Bool False) `shouldBe` "#f"

  describe "numbers" $ do
    it "displays an integer" $
      display (Real 42) `shouldBe` "42"
    it "displays a negative integer" $
      display (Real (-42)) `shouldBe` "-42"
    it "displays a rational" $
      display (Real (RRational (1 % 2))) `shouldBe` "1/2"
    it "displays a negative rational" $
      display (Real (RRational ((-1) % 2))) `shouldBe` "-1/2"
    it "displays a float" $
      display (Real (RFloat 1.5)) `shouldBe` "1.5"
    it "displays a complex number with a positive imaginary part" $
      display (Complex (1 :+ 2)) `shouldBe` "1+2i"
    it "displays a complex number with a negative imaginary part" $
      display (Complex (1 :+ (-2))) `shouldBe` "1-2i"
    it "displays a pure imaginary number" $
      display (Complex (0 :+ 2)) `shouldBe` "+2i"
    it "displays a complex number with no imaginary part as a real" $
      display (Complex (3 :+ 0)) `shouldBe` "3"
    it "displays float components" $
      display (Complex (RFloat 1.5 :+ RFloat 2.5)) `shouldBe` "1.5+2.5i"
    it "displays a negative float imaginary part" $
      display (Complex (RFloat 1.5 :+ RFloat (-2.5))) `shouldBe` "1.5-2.5i"
    it "displays a pure imaginary number with a float part" $
      display (Complex (0 :+ RFloat 1.5)) `shouldBe` "+1.5i"
    it "keeps an inexact zero imaginary part" $
      display (Complex (1 :+ RFloat 0)) `shouldBe` "1+0.0i"
    it "displays rational components" $
      display (Complex (RRational (1 % 2) :+ RRational (1 % 3))) `shouldBe` "1/2+1/3i"

  describe "characters" $ do
    it "displays a letter" $
      display (Char 'a') `shouldBe` "#\\a"
    it "displays a space by name" $
      display (Char ' ') `shouldBe` "#\\space"
    it "displays a newline by name" $
      display (Char '\n') `shouldBe` "#\\newline"

  describe "strings" $ do
    it "displays a plain string" $
      display (String "abc") `shouldBe` "\"abc\""
    it "escapes a double quote" $
      display (String "a\"b") `shouldBe` "\"a\\\"b\""
    it "escapes a backslash" $
      display (String "a\\b") `shouldBe` "\"a\\\\b\""
    it "escapes a newline" $
      display (String "a\nb") `shouldBe` "\"a\\nb\""

  describe "lists" $ do
    it "displays a flat list" $
      display (List [Atom "a", Atom "b"]) `shouldBe` "(a b)"
    it "displays an empty list" $
      display (List []) `shouldBe` "()"
    it "displays a nested list" $
      display (List [Atom "a", List [Atom "b"], Atom "c"]) `shouldBe` "(a (b) c)"
    it "displays a dotted list" $
      display (DottedList [Atom "a", Atom "b"] (Atom "c")) `shouldBe` "(a b . c)"

  describe "vectors" $ do
    it "displays a vector" $
      display (Vector (fromList [Number $ Int 1, Number $ Int 2])) `shouldBe` "#(1 2)"
    it "displays an empty vector" $
      display (Vector (fromList [])) `shouldBe` "#()"

  describe "round trips" $ do
    let roundTrips input =
          it ("round trips " ++ show input) $
            roundTrip input `shouldBe` Right (pack input)
    mapM_
      roundTrips
      [ "abc",
        "(a b c)",
        "(a b . c)",
        "#(1 2 3)",
        "1/2",
        "1.5",
        "1+2i",
        "#t",
        "\"a\\\"b\"",
        "#\\space",
        "(quote a)"
      ]
