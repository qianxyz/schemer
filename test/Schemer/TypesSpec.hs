{-# LANGUAGE OverloadedStrings #-}

module Schemer.TypesSpec (spec) where

import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))
import Data.Text (Text, pack)
import Data.Text.Display (display)
import Data.Vector (fromList)
import Schemer
import Test.Hspec
import Text.ParserCombinators.Parsec (parse)

-- | Name the branch a number matches, to check that a value built one
-- way is matched as its canonical representation.
branch :: Number -> String
branch (Int _) = "int"
branch (Rational _) = "rational"
branch (Float _) = "float"
branch (Complex _) = "complex"

-- | Parse a string, then display the result again.
roundTrip :: String -> Either String Text
roundTrip input = case parse parseExpr "" input of
  Left err -> Left (show err)
  Right val -> Right (display val)

spec :: Spec
spec = do
  describe "RealNum invariants" $ do
    it "demotes a rational with denominator 1 to an integer" $
      RRational (4 % 2) `shouldBe` RInt 2
    it "demotes a zero numerator to the exact zero" $
      RRational (0 % 5) `shouldBe` RInt 0
    it "demotes a negative rational with denominator 1" $
      RRational ((-4) % 2) `shouldBe` RInt (-2)
    it "keeps a rational that does not reduce to an integer" $
      RRational (4 % 6) `shouldBe` RRational (2 % 3)
    it "does not conflate an exact and an inexact integer" $
      RInt 1 `shouldNotBe` RFloat 1.0

  describe "RealNum arithmetic preserves the invariant" $ do
    it "demotes a sum of rationals" $
      RRational (1 % 2) + RRational (1 % 2) `shouldBe` RInt 1
    it "demotes a product of rationals" $
      RRational (2 % 3) * RRational (3 % 2) `shouldBe` RInt 1
    it "demotes the signum of a rational" $
      signum (RRational (1 % 2)) `shouldBe` RInt 1

  describe "Number invariants" $ do
    it "demotes a complex with an exact zero imaginary part" $
      Complex (RInt 1 :+ RInt 0) `shouldBe` Int 1
    it "demotes a complex that is exactly zero" $
      Complex (RInt 0 :+ RInt 0) `shouldBe` Int 0
    it "demotes a complex with an inexact real part" $
      Complex (RFloat 1.5 :+ RInt 0) `shouldBe` Float 1.5
    it "demotes when the imaginary part reduces to an exact zero" $
      Complex (RInt 1 :+ RRational (0 % 5)) `shouldBe` Int 1
    it "keeps a complex with an inexact zero imaginary part" $
      Complex (RInt 1 :+ RFloat 0) `shouldNotBe` Int 1
    it "keeps a complex with a non-zero imaginary part" $
      Complex (RInt 1 :+ RInt 2) `shouldNotBe` Int 1

  describe "matching follows the canonical representation" $ do
    it "matches a demoted rational as an integer" $
      branch (Rational (4 % 2)) `shouldBe` "int"
    it "matches a proper rational as a rational" $
      branch (Rational (1 % 2)) `shouldBe` "rational"
    it "matches a demoted complex as an integer" $
      branch (Complex (RInt 1 :+ RInt 0)) `shouldBe` "int"
    it "matches a complex with an inexact zero imaginary part as complex" $
      branch (Complex (RInt 1 :+ RFloat 0)) `shouldBe` "complex"

  describe "ratio" $ do
    it "rejects a zero denominator" $
      ratio 1 0 `shouldBe` Nothing
    it "demotes when the division is exact" $
      ratio 4 2 `shouldBe` Just (RInt 2)
    it "builds a proper rational" $
      ratio 1 2 `shouldBe` Just (RRational (1 % 2))
    it "normalises the sign onto the numerator" $
      ratio 1 (-2) `shouldBe` Just (RRational ((-1) % 2))

  describe "Display" $ do
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
