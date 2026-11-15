module Schemer.EvalSpec (spec) where

import Data.Complex (Complex ((:+)))
import Data.Either (isLeft, isRight)
import Data.Ratio ((%))
import Data.Vector (fromList)
import Schemer.Error (ThrowsError)
import Schemer.Eval (eval)
import Schemer.Parse (readExpr)
import Schemer.Types
import Test.Hspec

-- | Parse a string and evaluate the resulting expression.
evalString :: String -> ThrowsError SExp
evalString source = readExpr source >>= eval

-- | Assert that a source string evaluates to the given number.
evaluatesTo :: String -> Number -> Expectation
evaluatesTo source n = evalString source `shouldBe` Right (Number n)

infix 1 `evaluatesTo`

-- | Assert that evaluating a source string fails, with any error.
isError :: String -> Expectation
isError source = evalString source `shouldSatisfy` isLeft

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

  describe "if" $ do
    it "evaluates the consequent when the test is true" $
      "(if #t 1 2)" `evaluatesTo` Int 1
    it "evaluates the alternative when the test is false" $
      "(if #f 1 2)" `evaluatesTo` Int 2
    it "evaluates the test expression" $
      "(if (< 1 2) 1 2)" `evaluatesTo` Int 1
    it "evaluates the chosen branch" $
      "(if #t (+ 1 2) 0)" `evaluatesTo` Int 3
    it "treats 0 as true" $
      "(if 0 1 2)" `evaluatesTo` Int 1
    it "treats the empty list as true" $
      "(if '() 1 2)" `evaluatesTo` Int 1
    it "treats the empty string as true" $
      "(if \"\" 1 2)" `evaluatesTo` Int 1
    it "does not evaluate the alternative when the test is true" $
      "(if #t 1 (/ 1 0))" `evaluatesTo` Int 1
    it "does not evaluate the consequent when the test is false" $
      "(if #f (/ 1 0) 2)" `evaluatesTo` Int 2
    it "nests in the test position" $
      "(if (if #f #t #f) 1 2)" `evaluatesTo` Int 2
    it "evaluates the consequent of a two-armed if when the test is true" $
      "(if #t 1)" `evaluatesTo` Int 1
    it "does not evaluate the consequent of a two-armed if when the test is false" $
      evalString "(if #f (/ 1 0))" `shouldSatisfy` isRight
    it "propagates an error from the test" $
      isError "(if (/ 1 0) 1 2)"
    it "rejects an if with only a test" $
      isError "(if #t)"
    it "rejects an if with no arguments" $
      isError "(if)"
    it "rejects an if with too many arguments" $
      isError "(if #t 1 2 3)"

  describe "cond" $ do
    let evalsTo source v = evalString source `shouldBe` Right v
    it "picks the first clause with a true test" $
      "(cond ((> 3 2) 'greater) ((< 3 2) 'less))" `evalsTo` Atom "greater"
    it "falls through to else" $
      "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))" `evalsTo` Atom "equal"
    it "stops at the first true test" $
      "(cond (#t 1) (#t 2))" `evaluatesTo` Int 1
    it "treats any non-#f value as true" $
      "(cond (0 'zero) (else 'other))" `evalsTo` Atom "zero"
    it "returns the value of the last expression in a clause" $
      "(cond (#t 1 2 3))" `evaluatesTo` Int 3
    it "evaluates every expression in the chosen clause" $
      isError "(cond (#t (/ 1 0) 2))"
    it "returns the test value of a clause with only a test" $
      "(cond (#f) ((+ 1 2)))" `evaluatesTo` Int 3
    it "does not evaluate later tests" $
      "(cond (#t 1) ((/ 1 0) 2))" `evaluatesTo` Int 1
    it "does not evaluate bodies of false clauses" $
      "(cond (#f (/ 1 0)) (else 2))" `evaluatesTo` Int 2
    it "rejects a cond with no clauses" $
      isError "(cond)"
    it "rejects an else clause that is not last" $
      isError "(cond (else 1) (#t 2))"
    it "rejects an empty else clause" $
      isError "(cond (else))"
    it "rejects a clause that is not a list" $
      isError "(cond 1)"

  describe "case" $ do
    let evalsTo source v = evalString source `shouldBe` Right v
    it "picks the clause containing the key" $
      "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))" `evalsTo` Atom "composite"
    it "falls through to else" $
      "(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))"
        `evalsTo` Atom "consonant"
    it "matches symbols" $
      "(case 'b ((a) 1) ((b) 2))" `evaluatesTo` Int 2
    it "matches characters" $
      "(case #\\x ((#\\x) 1) (else 2))" `evaluatesTo` Int 1
    it "compares numbers by eqv?, so exactness matters" $
      "(case 2.0 ((2) 'exact) (else 'other))" `evalsTo` Atom "other"
    it "does not evaluate the data" $
      "(case '+ ((+) 'plus) (else 'other))" `evalsTo` Atom "plus"
    it "returns the value of the last expression in a clause" $
      "(case 1 ((1) 'a 'b))" `evalsTo` Atom "b"
    it "evaluates the key" $
      "(case (+ 1 1) ((2) 'two) (else 'other))" `evalsTo` Atom "two"
    it "never matches a clause with no data" $
      "(case 1 (() 'none) (else 'other))" `evalsTo` Atom "other"
    it "rejects a case with no clauses" $
      isError "(case 1)"
    it "rejects an else clause that is not last" $
      isError "(case 1 (else 1) ((1) 2))"
    it "rejects a clause without a body" $
      isError "(case 1 ((1)))"

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
    it "modulo by zero is an error" $
      isError "(modulo 7 0)"
    it "quotient by zero is an error" $
      isError "(quotient 7 0)"
    it "remainder by zero is an error" $
      isError "(remainder 7 0)"
    it "allows a zero dividend" $
      "(modulo 0 7)" `evaluatesTo` Int 0

  describe "numerical comparisons" $ do
    let isTrue source = evalString source `shouldBe` Right (Bool True)
        isFalse source = evalString source `shouldBe` Right (Bool False)
    describe "=" $ do
      it "accepts equal integers" $ isTrue "(= 1 1)"
      it "rejects different integers" $ isFalse "(= 1 2)"
      it "holds across exactness" $ isTrue "(= 1 1.0)"
      it "compares an exact rational with a float" $ isTrue "(= 1/2 0.5)"
      it "compares exactly rather than through doubles" $
        isFalse "(= 9007199254740993 9007199254740992.0)"
      it "compares complex numbers" $ isTrue "(= 1+2i 1+2i)"
      it "treats an inexact zero imaginary part as real" $ isTrue "(= 1+0.0i 1)"
      it "chains over all arguments" $ isFalse "(= 1 1 2)"
    describe "<" $ do
      it "accepts an increasing sequence" $ isTrue "(< 1 2 3)"
      it "rejects a repeated value" $ isFalse "(< 1 2 2)"
      it "compares across exactness" $ isTrue "(< 1/3 0.34)"
    describe ">" $ do
      it "accepts a decreasing sequence" $ isTrue "(> 3 2.5 1)"
      it "rejects an increasing pair" $ isFalse "(> 1 2)"
    describe "<=" $ do
      it "accepts a non-decreasing sequence" $ isTrue "(<= 1 2 2)"
      it "rejects a decrease" $ isFalse "(<= 2 1)"
    describe ">=" $ do
      it "accepts a non-increasing sequence" $ isTrue "(>= 2 2 1)"
      it "rejects an increase" $ isFalse "(>= 1 2)"
    describe "errors" $ do
      it "requires at least two arguments" $
        isError "(< 1)"
      it "rejects a complex argument to an ordering" $
        isError "(< 1 1+i)"
      it "type-checks every argument before comparing" $
        isError "(< 2 1 'a)"

  describe "string comparisons" $ do
    let isTrue source = evalString source `shouldBe` Right (Bool True)
        isFalse source = evalString source `shouldBe` Right (Bool False)
    it "string=? accepts equal strings" $ isTrue "(string=? \"abc\" \"abc\")"
    it "string=? is case sensitive" $ isFalse "(string=? \"abc\" \"ABC\")"
    it "string<? orders lexicographically" $ isTrue "(string<? \"abc\" \"abd\")"
    it "string<? puts a prefix first" $ isTrue "(string<? \"ab\" \"abc\")"
    it "string<? rejects equal strings" $ isFalse "(string<? \"abc\" \"abc\")"
    it "string>? accepts a greater string" $ isTrue "(string>? \"b\" \"abc\")"
    it "string<=? accepts equal strings" $ isTrue "(string<=? \"abc\" \"abc\")"
    it "string>=? rejects a smaller string" $ isFalse "(string>=? \"abc\" \"abd\")"
    it "string-ci=? ignores case" $ isTrue "(string-ci=? \"abc\" \"ABC\")"
    it "string-ci<? ignores case" $ isTrue "(string-ci<? \"abc\" \"ABD\")"
    it "string<? is case sensitive where string-ci<? is not" $
      isTrue "(string<? \"B\" \"a\")"
    it "string-ci>? ignores case" $ isTrue "(string-ci>? \"B\" \"a\")"
    it "requires string arguments" $
      isError "(string=? \"a\" 'a)"
    it "requires at least two arguments" $
      isError "(string=? \"a\")"

  describe "pairs and lists" $ do
    let n = Number . Int
        evalsTo source v = evalString source `shouldBe` Right v
    describe "car" $ do
      it "returns the first element of a list" $
        "(car '(1 2 3))" `evalsTo` n 1
      it "returns a list element unevaluated" $
        "(car '((1) 2))" `evalsTo` List [n 1]
      it "returns the first element of a dotted list" $
        "(car '(1 . 2))" `evalsTo` n 1
      it "rejects the empty list" $
        isError "(car '())"
      it "rejects a non-list" $
        isError "(car 'a)"
      it "rejects two arguments" $
        isError "(car '(1) '(2))"
    describe "cdr" $ do
      it "returns the rest of a list" $
        "(cdr '(1 2 3))" `evalsTo` List [n 2, n 3]
      it "returns the empty list for a single-element list" $
        "(cdr '(1))" `evalsTo` List []
      it "returns the tail of a pair" $
        "(cdr '(1 . 2))" `evalsTo` n 2
      it "returns the rest of a longer dotted list" $
        "(cdr '(1 2 . 3))" `evalsTo` DottedList [n 2] (n 3)
      it "rejects the empty list" $
        isError "(cdr '())"
      it "rejects a vector" $
        isError "(cdr '#(1 2))"
      it "rejects no arguments" $
        isError "(cdr)"
    describe "cons" $ do
      it "prepends to a list" $
        "(cons 1 '(2 3))" `evalsTo` List [n 1, n 2, n 3]
      it "builds a single-element list from the empty list" $
        "(cons 1 '())" `evalsTo` List [n 1]
      it "builds a pair from two non-lists" $
        "(cons 1 2)" `evalsTo` DottedList [n 1] (n 2)
      it "prepends to a dotted list" $
        "(cons 1 '(2 . 3))" `evalsTo` DottedList [n 1, n 2] (n 3)
      it "conses a list onto a list" $
        "(cons '(1) '(2))" `evalsTo` List [List [n 1], n 2]
      it "evaluates its arguments" $
        "(cons (+ 1 1) '())" `evalsTo` List [n 2]
      it "rejects one argument" $
        isError "(cons 1)"
    describe "canonical form" $ do
      it "conses onto a dotted list ending in a list the same as onto a list" $
        "(cons 0 '(1 . (2)))" `evalsTo` List [n 0, n 1, n 2]
      it "takes the cdr of a normalized dotted list as a list" $
        "(cdr '(1 . (2 3)))" `evalsTo` List [n 2, n 3]
      it "treats (a . ()) as a proper list" $
        "(null? (cdr '(1 . ())))" `evalsTo` Bool True
    describe "round trips" $ do
      it "rebuilds a list from its car and cdr" $
        "(cons (car '(1 2 3)) (cdr '(1 2 3)))" `evalsTo` List [n 1, n 2, n 3]
      it "rebuilds a dotted list from its car and cdr" $
        "(cons (car '(1 2 . 3)) (cdr '(1 2 . 3)))" `evalsTo` DottedList [n 1, n 2] (n 3)
      it "takes apart a cons with car" $
        "(car (cons 1 2))" `evalsTo` n 1
      it "takes apart a cons with cdr" $
        "(cdr (cons 1 2))" `evalsTo` n 2

  describe "equivalence predicates" $ do
    let isTrue source = evalString source `shouldBe` Right (Bool True)
        isFalse source = evalString source `shouldBe` Right (Bool False)
    describe "eqv?" $ do
      it "accepts the same symbol" $ isTrue "(eqv? 'a 'a)"
      it "rejects different symbols" $ isFalse "(eqv? 'a 'b)"
      it "accepts the same boolean" $ isTrue "(eqv? #f #f)"
      it "rejects different booleans" $ isFalse "(eqv? #t #f)"
      it "accepts the same character" $ isTrue "(eqv? #\\a #\\a)"
      it "accepts equal exact numbers" $ isTrue "(eqv? 2 2)"
      it "accepts equal rationals" $ isTrue "(eqv? 1/2 2/4)"
      it "accepts equal inexact numbers" $ isTrue "(eqv? 1.5 1.5)"
      it "rejects numbers of different exactness" $ isFalse "(eqv? 2 2.0)"
      it "accepts two empty lists" $ isTrue "(eqv? '() '())"
      it "rejects freshly allocated pairs" $ isFalse "(eqv? (cons 1 2) (cons 1 2))"
      it "rejects values of different types" $ isFalse "(eqv? 'a \"a\")"
      it "requires two arguments" $
        isError "(eqv? 1)"
    describe "eq?" $ do
      it "accepts the same symbol" $ isTrue "(eq? 'a 'a)"
      it "accepts two empty lists" $ isTrue "(eq? '() '())"
      it "rejects freshly allocated pairs" $ isFalse "(eq? (cons 1 2) (cons 1 2))"
    describe "equal?" $ do
      it "accepts structurally equal lists" $ isTrue "(equal? '(a (b) c) '(a (b) c))"
      it "rejects lists of different lengths" $ isFalse "(equal? '(a b) '(a b c))"
      it "accepts equal dotted lists" $ isTrue "(equal? '(1 2 . 3) (cons 1 (cons 2 3)))"
      it "rejects dotted lists with different tails" $ isFalse "(equal? '(1 . 2) '(1 . 3))"
      it "rejects a list against a dotted list" $ isFalse "(equal? '(1 2) '(1 . 2))"
      it "accepts equal strings" $ isTrue "(equal? \"abc\" \"abc\")"
      it "rejects different strings" $ isFalse "(equal? \"abc\" \"abd\")"
      it "accepts equal vectors" $ isTrue "(equal? '#(1 (2) \"x\") '#(1 (2) \"x\"))"
      it "rejects vectors of different lengths" $ isFalse "(equal? '#(1 2) '#(1))"
      it "compares numbers inside lists with eqv?" $ isFalse "(equal? '(2) '(2.0))"
      it "falls back to eqv? on atoms" $ isTrue "(equal? 'a 'a)"
      it "rejects a list against a vector" $ isFalse "(equal? '(1 2) '#(1 2))"

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

  describe "symbol conversions" $ do
    it "converts a symbol to its name" $
      evalString "(symbol->string 'abc)" `shouldBe` Right (String "abc")
    it "converts a string to a symbol" $
      evalString "(string->symbol \"abc\")" `shouldBe` Right (Atom "abc")
    it "preserves case when converting a string to a symbol" $
      evalString "(string->symbol \"ABC\")" `shouldBe` Right (Atom "ABC")
    it "builds a symbol that could not be written as a literal" $
      evalString "(string->symbol \"hello world\")" `shouldBe` Right (Atom "hello world")
    it "round trips a string through a symbol" $
      evalString "(symbol->string (string->symbol \"x\"))" `shouldBe` Right (String "x")
    it "produces a value that symbol? accepts" $
      evalString "(symbol? (string->symbol \"abc\"))" `shouldBe` Right (Bool True)
