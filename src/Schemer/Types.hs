{-# LANGUAGE OverloadedStrings #-}

module Schemer.Types where

import Data.Complex (Complex, imagPart, realPart)
import Data.Ratio (denominator, numerator)
import Data.Text.Display (Display (displayBuilder, displayList))
import Data.Vector (Vector, toList)

data SExp
  = Atom String
  | Real RealNum
  | Complex (Complex RealNum)
  | String String
  | Bool Bool
  | Char Char
  | List [SExp]
  | -- | A list ending in a non-nil element, for example
    -- @(a b . c)@ (shorthand for @(cons a (cons b c))@).
    DottedList [SExp] SExp
  | -- | A data structure indexed by range of integers
    -- starting from 0. Represented as e.g. @#(a b c)@.
    Vector (Vector SExp)
  deriving (Show, Eq)

instance Display SExp where
  displayBuilder (Atom name) = displayBuilder name
  displayBuilder (Real n) = displayBuilder n
  displayBuilder (Complex n) = displayComplex (realPart n) (imagPart n)
    where
      displayComplex real 0 = displayBuilder real
      displayComplex real imag =
        (if real == 0 then "" else displayBuilder real)
          <> (if signum imag == 1 then "+" else "")
          <> displayBuilder imag
          <> "i"
  displayBuilder (String s) = "\"" <> foldMap escapeChar s <> "\""
    where
      escapeChar = displayBuilder . charString
      charString c = maybe [c] (\n -> ['\\', n]) (lookup c escapes)
  displayBuilder (Bool True) = "#t"
  displayBuilder (Bool False) = "#f"
  displayBuilder (Char c) = "#\\" <> displayBuilder charLookup
    where
      charLookup = maybe [c] id $ lookup c charNames
  displayBuilder (List exps) = "(" <> displayList exps <> ")"
  displayBuilder (DottedList init' last') =
    "(" <> displayList init' <> " . " <> displayBuilder last' <> ")"
  displayBuilder (Vector v) = "#(" <> displayList (toList v) <> ")"

  displayList [] = ""
  displayList (x : xs) = displayBuilder x <> foldMap go xs
    where
      go y = " " <> displayBuilder y

-- | Escape characters, with the letter after @\\@.
escapes :: [(Char, Char)]
escapes =
  [ ('\n', 'n'),
    ('\r', 'r'),
    ('\t', 't'),
    ('"', '"'),
    ('\\', '\\')
  ]

-- | Character names, with the character they represent.
charNames :: [(Char, String)]
charNames =
  [ (' ', "space"),
    ('\n', "newline")
  ]

-- | A real number in Scheme, which can be an integer, a rational
-- or a float. Also used as the real/imag parts of a complex number.
data RealNum
  = Int Integer
  | Rational Rational
  | Float Double
  deriving (Show, Eq)

instance Display RealNum where
  displayBuilder (Int n) = displayBuilder n
  displayBuilder (Rational r) =
    displayBuilder (numerator r)
      <> "/"
      <> displayBuilder (denominator r)
  displayBuilder (Float n) = displayBuilder n

-- TODO: Make `Complex RealNum` instance of `Num`.
-- Can be done by implementing `RealFloat` for `RealNum`
-- or writing the arithmetics directly.

isFloat :: RealNum -> Bool
isFloat (Float _) = True
isFloat _ = False

-- | Build a rational, demoting to Int when the denominator is 1.
fromRatio :: Rational -> RealNum
fromRatio r
  | denominator r == 1 = Int (numerator r)
  | otherwise = Rational r

toRatio :: RealNum -> Rational
toRatio (Int a) = fromInteger a
toRatio (Rational a) = a
toRatio (Float a) = toRational a

toDouble :: RealNum -> Double
toDouble (Int a) = fromInteger a
toDouble (Rational a) = fromRational a
toDouble (Float a) = a

-- | Lift unary operations on Integer, Rational, and Double to RealNum.
liftUnary ::
  (Integer -> Integer) ->
  (Rational -> Rational) ->
  (Double -> Double) ->
  RealNum ->
  RealNum
liftUnary fi _ _ (Int a) = Int (fi a)
liftUnary _ fr _ (Rational a) = fromRatio (fr a)
liftUnary _ _ fd (Float a) = Float (fd a)

-- | Lift binary operations on Integer, Rational, and Double to RealNum.
liftBinary ::
  (Integer -> Integer -> Integer) ->
  (Rational -> Rational -> Rational) ->
  (Double -> Double -> Double) ->
  RealNum ->
  RealNum ->
  RealNum
liftBinary fi fr fd a b
  | Int x <- a, Int y <- b = Int (fi x y)
  | isFloat a || isFloat b = Float (fd (toDouble a) (toDouble b))
  | otherwise = fromRatio (fr (toRatio a) (toRatio b))

instance Num RealNum where
  fromInteger = Int
  (+) = liftBinary (+) (+) (+)
  (-) = liftBinary (-) (-) (-)
  (*) = liftBinary (*) (*) (*)
  abs = liftUnary abs abs abs
  signum = liftUnary signum signum signum
  negate = liftUnary negate negate negate

instance Fractional RealNum where
  fromRational = Rational

  a / b
    | isFloat a || isFloat b = Float (toDouble a / toDouble b)
    -- int / int should produce a rational
    | otherwise = fromRatio (toRatio a / toRatio b)
