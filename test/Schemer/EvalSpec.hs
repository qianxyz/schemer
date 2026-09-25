module Schemer.EvalSpec (spec) where

import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))
import Data.Vector (fromList)
import Schemer.Eval (eval)
import Schemer.Parse (readExpr)
import Schemer.Types
import Test.Hspec

-- | Parse a string and evaluate the resulting expression.
evalString :: String -> Either String SExp
evalString source = eval <$> readExpr source

-- | Assert that a source string evaluates to the given number.
evaluatesTo :: String -> Number -> Expectation
evaluatesTo source n = evalString source `shouldBe` Right (Number n)

infix 1 `evaluatesTo`

spec :: Spec
spec = do
  describe "self-evaluating expressions" $ do
    it "evaluates a number to itself" $
      "42" `evaluatesTo` Int 42
    it "evaluates a string to itself" $
      evalString "\"abc\"" `shouldBe` Right (String "abc")
    it "evaluates a boolean to itself" $
      evalString "#t" `shouldBe` Right (Bool True)
    it "evaluates a character to itself" $
      evalString "#\\a" `shouldBe` Right (Char 'a')
    it "evaluates a vector to itself" $
      evalString "#(1 2)" `shouldBe` Right (Vector (fromList [Number (Int 1), Number (Int 2)]))
    it "does not evaluate the elements of a vector" $
      evalString "#(1 (+ 1 2))"
        `shouldBe` Right (Vector (fromList [Number (Int 1), List [Atom "+", Number (Int 1), Number (Int 2)]]))

  describe "quote" $ do
    it "returns a quoted atom unevaluated" $
      evalString "(quote a)" `shouldBe` Right (Atom "a")
    it "returns a quoted list unevaluated" $
      evalString "(quote (+ 1 2))"
        `shouldBe` Right (List [Atom "+", Number (Int 1), Number (Int 2)])
    it "treats the ' prefix the same as quote" $
      evalString "'(a b)" `shouldBe` Right (List [Atom "a", Atom "b"])

  describe "+" $ do
    it "returns 0 with no arguments" $
      "(+)" `evaluatesTo` Int 0
    it "returns its single argument" $
      "(+ 5)" `evaluatesTo` Int 5
    it "adds several integers" $
      "(+ 1 2 3)" `evaluatesTo` Int 6
    it "demotes an exact rational sum to an integer" $
      "(+ 1/2 1/2)" `evaluatesTo` Int 1
    it "becomes inexact when any argument is inexact" $
      "(+ 1 2.5)" `evaluatesTo` Float 3.5
    it "demotes a complex sum with a zero imaginary part" $
      "(+ 1+2i 3-2i)" `evaluatesTo` Int 4

  describe "-" $ do
    it "negates a single argument" $
      "(- 5)" `evaluatesTo` Int (-5)
    it "negates a single rational" $
      "(- 1/2)" `evaluatesTo` Rational ((-1) % 2)
    it "subtracts from left to right" $
      "(- 10 1 2)" `evaluatesTo` Int 7

  describe "*" $ do
    it "returns 1 with no arguments" $
      "(*)" `evaluatesTo` Int 1
    it "multiplies several integers" $
      "(* 2 3 4)" `evaluatesTo` Int 24
    it "becomes inexact when any argument is inexact" $
      "(* 2 0.5)" `evaluatesTo` Float 1.0
    it "demotes a complex product with a zero imaginary part" $
      "(* 1+i 1-i)" `evaluatesTo` Int 2

  describe "/" $ do
    it "takes the reciprocal of a single argument" $
      "(/ 2)" `evaluatesTo` Rational (1 % 2)
    it "demotes an exact quotient to an integer" $
      "(/ 6 3)" `evaluatesTo` Int 2
    it "keeps a non-integer exact quotient as a rational" $
      "(/ 1 3)" `evaluatesTo` Rational (1 % 3)
    it "divides from left to right" $
      "(/ 12 2 3)" `evaluatesTo` Int 2
    it "becomes inexact when any argument is inexact" $
      "(/ 1 2.0)" `evaluatesTo` Float 0.5
    it "divides exact complex numbers exactly" $
      "(/ 1+2i 3+4i)" `evaluatesTo` Complex (RRational (11 % 25) :+ RRational (2 % 25))

  describe "integer division" $ do
    it "quotient truncates toward zero" $
      "(quotient -7 2)" `evaluatesTo` Int (-3)
    it "remainder takes the sign of the dividend" $
      "(remainder -7 2)" `evaluatesTo` Int (-1)
    it "modulo takes the sign of the divisor" $
      "(modulo -7 2)" `evaluatesTo` Int 1
    it "modulo with a negative divisor" $
      "(modulo 7 -2)" `evaluatesTo` Int (-1)

  describe "nested expressions" $ do
    it "evaluates arguments before applying" $
      "(+ 1 (* 2 3))" `evaluatesTo` Int 7
    it "evaluates deeply nested arithmetic" $
      "(/ (- 10 (* 2 3)) (+ 1 1))" `evaluatesTo` Int 2

  describe "type predicates" $ do
    let isTrue source = evalString source `shouldBe` Right (Bool True)
        isFalse source = evalString source `shouldBe` Right (Bool False)
    describe "boolean?" $ do
      it "accepts #t" $ isTrue "(boolean? #t)"
      it "accepts #f" $ isTrue "(boolean? #f)"
      it "rejects a number" $ isFalse "(boolean? 0)"
    describe "symbol?" $ do
      it "accepts a quoted symbol" $ isTrue "(symbol? 'a)"
      it "accepts a quoted peculiar identifier" $ isTrue "(symbol? '+)"
      it "rejects a string" $ isFalse "(symbol? \"a\")"
    describe "char?" $ do
      it "accepts a character" $ isTrue "(char? #\\a)"
      it "rejects a one-character string" $ isFalse "(char? \"a\")"
    describe "vector?" $ do
      it "accepts a vector" $ isTrue "(vector? '#(1 2))"
      it "accepts an empty vector" $ isTrue "(vector? '#())"
      it "rejects a list" $ isFalse "(vector? '(1 2))"
    describe "pair?" $ do
      it "accepts a non-empty list" $ isTrue "(pair? '(1 2))"
      it "accepts a single-element list" $ isTrue "(pair? '(1))"
      it "accepts a dotted list" $ isTrue "(pair? '(1 . 2))"
      it "rejects the empty list" $ isFalse "(pair? '())"
      it "rejects a vector" $ isFalse "(pair? '#(1 2))"
      it "rejects a symbol" $ isFalse "(pair? 'a)"
    describe "number?" $ do
      it "accepts an integer" $ isTrue "(number? 1)"
      it "accepts a rational" $ isTrue "(number? 1/2)"
      it "accepts a float" $ isTrue "(number? 1.5)"
      it "accepts a complex number" $ isTrue "(number? 1+2i)"
      it "rejects a quoted symbol" $ isFalse "(number? 'a)"
    describe "string?" $ do
      it "accepts a string" $ isTrue "(string? \"abc\")"
      it "accepts the empty string" $ isTrue "(string? \"\")"
      it "rejects a character" $ isFalse "(string? #\\a)"
    describe "null?" $ do
      it "accepts the empty list" $ isTrue "(null? '())"
      it "rejects a non-empty list" $ isFalse "(null? '(1))"
      it "rejects a dotted list" $ isFalse "(null? '(1 . 2))"
      it "rejects an empty vector" $ isFalse "(null? '#())"
