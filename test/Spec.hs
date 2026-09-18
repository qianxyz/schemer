module Main (main) where

import Data.Complex (Complex ((:+)))
import Data.Either (isLeft)
import Data.Ratio ((%))
import Data.Vector (fromList)
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
    it "parses an identifier with digits" $
      parseSExp "add1" `shouldBe` Right (Atom "add1")
    it "parses an identifier with a trailing special character" $
      parseSExp "lat?" `shouldBe` Right (Atom "lat?")
    it "parses an identifier ending in +" $
      parseSExp "o+" `shouldBe` Right (Atom "o+")
    it "parses an identifier containing -> " $
      parseSExp "list->vector" `shouldBe` Right (Atom "list->vector")
    it "parses an identifier containing @ and ." $
      parseSExp "x@y.z" `shouldBe` Right (Atom "x@y.z")
    it "rejects an identifier starting with @" $
      parseSExp "@x" `shouldSatisfy` isLeft
    describe "peculiar identifiers" $ do
      it "parses + as an atom" $
        parseSExp "+" `shouldBe` Right (Atom "+")
      it "parses - as an atom" $
        parseSExp "-" `shouldBe` Right (Atom "-")
      it "parses ... as an atom" $
        parseSExp "..." `shouldBe` Right (Atom "...")
      it "parses ... inside a list" $
        parseSExp "(a ...)" `shouldBe` Right (List [Atom "a", Atom "..."])
      it "parses + as the operator in a call" $
        parseSExp "(+ 1 2)" `shouldBe` Right (List [Atom "+", Real 1, Real 2])
      it "rejects a sign followed by a letter" $
        parseSExp "+a" `shouldSatisfy` isLeft
      it "rejects four dots" $
        parseSExp "...." `shouldSatisfy` isLeft

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
    it "parses a punctuation character" $
      parseSExp "#\\(" `shouldBe` Right (Char '(')
    it "parses a literal space character" $
      parseSExp "#\\ " `shouldBe` Right (Char ' ')
    it "parses newline" $
      parseSExp "#\\newline" `shouldBe` Right (Char '\n')
    it "parses a single letter that prefixes a character name" $
      parseSExp "#\\s" `shouldBe` Right (Char 's')
    it "parses characters inside a list" $
      parseSExp "(#\\a #\\b)" `shouldBe` Right (List [Char 'a', Char 'b'])
    it "rejects an unknown character name" $
      parseSExp "#\\ab" `shouldSatisfy` isLeft
    it "rejects a character name followed by garbage" $
      parseSExp "#\\spacex" `shouldSatisfy` isLeft
    it "rejects a prefix of a character name" $
      parseSExp "#\\sp" `shouldSatisfy` isLeft
    it "rejects an empty character literal" $
      parseSExp "#\\" `shouldSatisfy` isLeft

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
        parseSExp "1/2+1/3i"
          `shouldBe` Right (Complex (Rational (1 % 2) :+ Rational (1 % 3)))
      it "parses mixed negative components" $
        parseSExp "-1/2-1.5i"
          `shouldBe` Right (Complex (Rational ((-1) % 2) :+ Float (-1.5)))
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

    describe "lists" $ do
      it "parses a flat list" $
        parseSExp "(a test)" `shouldBe` Right (List [Atom "a", Atom "test"])
      it "parses a nested list" $
        parseSExp "(a (nested) test)"
          `shouldBe` Right (List [Atom "a", List [Atom "nested"], Atom "test"])
      it "parses a dotted list" $
        parseSExp "(a (dotted . list) test)"
          `shouldBe` Right
            (List [Atom "a", DottedList [Atom "dotted"] (Atom "list"), Atom "test"])
      it "parses a quoted list" $
        parseSExp "(a '(quoted (dotted . list)) test)"
          `shouldBe` Right
            ( List
                [ Atom "a",
                  List
                    [ Atom "quote",
                      List
                        [ Atom "quoted",
                          DottedList [Atom "dotted"] (Atom "list")
                        ]
                    ],
                  Atom "test"
                ]
            )
      it "parses an empty list" $
        parseSExp "()" `shouldBe` Right (List [])
      it "parses a dotted list with several leading elements" $
        parseSExp "(a b . c)" `shouldBe` Right (DottedList [Atom "a", Atom "b"] (Atom "c"))
      it "rejects imbalanced parens" $
        parseSExp "(a '(imbalanced parens)" `shouldSatisfy` isLeft
      it "rejects a dot with nothing before it" $
        parseSExp "(. a)" `shouldSatisfy` isLeft
      it "rejects a dot with nothing after it" $
        parseSExp "(a .)" `shouldSatisfy` isLeft
      it "rejects more than one element after the dot" $
        parseSExp "(a . b c)" `shouldSatisfy` isLeft

    describe "vectors" $ do
      it "parses a flat vector" $
        parseSExp "#(1 2 3)" `shouldBe` Right (Vector (fromList [Real 1, Real 2, Real 3]))
      it "parses an empty vector" $
        parseSExp "#()" `shouldBe` Right (Vector (fromList []))
      it "parses nested lists and vectors" $
        parseSExp "#(a (b c) #(d))"
          `shouldBe` Right
            ( Vector
                ( fromList
                    [ Atom "a",
                      List [Atom "b", Atom "c"],
                      Vector (fromList [Atom "d"])
                    ]
                )
            )
      it "rejects a dotted vector" $
        parseSExp "#(1 . 2)" `shouldSatisfy` isLeft

    describe "quote forms" $ do
      it "parses quote" $
        parseSExp "'a" `shouldBe` Right (List [Atom "quote", Atom "a"])
      it "parses quasiquote" $
        parseSExp "`a" `shouldBe` Right (List [Atom "quasiquote", Atom "a"])
      it "parses unquote" $
        parseSExp ",a" `shouldBe` Right (List [Atom "unquote", Atom "a"])
      it "parses unquote-splicing" $
        parseSExp ",@a" `shouldBe` Right (List [Atom "unquote-splicing", Atom "a"])
      it "parses unquotes inside a quasiquoted list" $
        parseSExp "`(a ,b ,@c)"
          `shouldBe` Right
            ( List
                [ Atom "quasiquote",
                  List
                    [ Atom "a",
                      List [Atom "unquote", Atom "b"],
                      List [Atom "unquote-splicing", Atom "c"]
                    ]
                ]
            )
      it "rejects a quote with nothing after it" $
        parseSExp "'" `shouldSatisfy` isLeft
