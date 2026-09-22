{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Schemer.Types
  ( -- * Scheme expressions
    SExp (..),

    -- * Numbers
    Number,
    pattern Int,
    pattern Rational,
    pattern Float,
    pattern Real,
    pattern Complex,
    -- toComplex,

    -- * Real numbers
    RealNum,
    pattern RInt,
    pattern RRational,
    pattern RFloat,
    ratio,
    -- isFloat,
    -- isNonNegative,
    -- toRatio,
    -- toDouble,
    -- liftUnary,
    -- liftBinary,

    -- * Lexical tables
    escapes,
    charNames,
  )
where

import Data.Complex (Complex ((:+)))
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Text.Display (Display (displayBuilder, displayList))
import Data.Vector (Vector, toList)

data SExp
  = Atom String
  | Number Number
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
  displayBuilder (Number n) = displayBuilder n
  displayBuilder (String s) = "\"" <> foldMap escapeChar s <> "\""
    where
      escapeChar = displayBuilder . charString
      charString c = maybe [c] (\n -> ['\\', n]) (lookup c escapes)
  displayBuilder (Bool True) = "#t"
  displayBuilder (Bool False) = "#f"
  displayBuilder (Char c) = "#\\" <> displayBuilder charLookup
    where
      charLookup = fromMaybe [c] $ lookup c charNames
  displayBuilder (List exps) = "(" <> displayList exps <> ")"
  displayBuilder (DottedList init' last') =
    "(" <> displayList init' <> " . " <> displayBuilder last' <> ")"
  displayBuilder (Vector v) = "#(" <> displayList (toList v) <> ")"

  displayList = mconcat . intersperse " " . map displayBuilder

-- | A number in Scheme. Can be complex or real.
--
-- Invariant: A complex number cannot have its imaginary part be
-- the exact 0. Such value will be demoted to a real number.
data Number
  = MkComplex (Complex RealNum)
  | MkReal RealNum
  deriving (Show, Eq)

pattern Int :: Integer -> Number
pattern Int n = MkReal (RInt n)

pattern Rational :: Rational -> Number
pattern Rational r = MkReal (RRational r)

pattern Float :: Double -> Number
pattern Float f = MkReal (RFloat f)

pattern Real :: RealNum -> Number
pattern Real r = MkReal r

pattern Complex :: Complex RealNum -> Number
pattern Complex c <- MkComplex c
  where
    Complex (re :+ RInt 0) = MkReal re
    Complex c = MkComplex c

{-# COMPLETE Int, Rational, Float, Complex #-}

{-# COMPLETE Real, Complex #-}

instance Display Number where
  displayBuilder (Real n) = displayBuilder n
  displayBuilder (Complex (re :+ im)) =
    (if re == 0 then "" else displayBuilder re)
      <> (if isNonNegative im then "+" else "")
      <> displayBuilder im
      <> "i"

-- | Convert a Scheme number to a complex number.
toComplex :: Number -> Complex RealNum
toComplex (Complex c) = c
toComplex (Real r) = r :+ 0

-- TODO: Implement Num for Number

-- | A real number in Scheme, which can be an integer, a rational
-- or a float. Also used as the real/imag parts of a complex number.
--
-- Invariant: A rational number cannot have its denominator be 1.
-- Such value will be demoted to an integer. As a result, a rational
-- number also cannot have its numerator be 0, so the integer 0 is
-- the unique representation of the exact 0.
data RealNum
  = MkInt Integer
  | MkRational Rational
  | MkFloat Double
  deriving (Show, Eq)

pattern RInt :: Integer -> RealNum
pattern RInt n = MkInt n

pattern RRational :: Rational -> RealNum
pattern RRational r <- MkRational r
  where
    RRational r
      | denominator r == 1 = MkInt (numerator r)
      | otherwise = MkRational r

pattern RFloat :: Double -> RealNum
pattern RFloat f = MkFloat f

{-# COMPLETE RInt, RRational, RFloat #-}

instance Display RealNum where
  displayBuilder (RInt n) = displayBuilder n
  displayBuilder (RRational r) =
    displayBuilder (numerator r)
      <> "/"
      <> displayBuilder (denominator r)
  displayBuilder (RFloat n) = displayBuilder n

isFloat :: RealNum -> Bool
isFloat (RFloat _) = True
isFloat _ = False

isNonNegative :: RealNum -> Bool
isNonNegative (RInt n) = n >= 0
isNonNegative (RRational n) = n >= 0
isNonNegative (RFloat n) = n >= 0

-- | Compose a rational (or int) from two integers.
-- Return @Nothing@ when the denominator is 0.
ratio :: Integer -> Integer -> Maybe RealNum
ratio _ 0 = Nothing
ratio num denom = Just $ RRational (num % denom)

-- | Convert a Scheme real number to a Haskell rational.
--
-- Note that this function is exact: Specifically, converting a float
-- to a rational is exact, in the sense that IEEE floating point numbers
-- that are finite are all rationals.
toRatio :: RealNum -> Rational
toRatio (RInt a) = fromInteger a
toRatio (RRational a) = a
toRatio (RFloat a) = toRational a

-- | Convert a Scheme real number to a Haskell double.
-- Note that this is a lossy conversion.
toDouble :: RealNum -> Double
toDouble (RInt a) = fromInteger a
toDouble (RRational a) = fromRational a
toDouble (RFloat a) = a

-- | Lift unary operations on Integer, Rational, and Double to RealNum.
liftUnary ::
  (Integer -> Integer) ->
  (Rational -> Rational) ->
  (Double -> Double) ->
  RealNum ->
  RealNum
liftUnary fi _ _ (RInt a) = RInt (fi a)
liftUnary _ fr _ (RRational a) = RRational (fr a)
liftUnary _ _ fd (RFloat a) = RFloat (fd a)

-- | Lift binary operations on Integer, Rational, and Double to RealNum.
liftBinary ::
  (Integer -> Integer -> Integer) ->
  (Rational -> Rational -> Rational) ->
  (Double -> Double -> Double) ->
  RealNum ->
  RealNum ->
  RealNum
liftBinary fi fr fd a b
  | RInt x <- a, RInt y <- b = RInt (fi x y)
  | isFloat a || isFloat b = RFloat (fd (toDouble a) (toDouble b))
  | otherwise = RRational (fr (toRatio a) (toRatio b))

instance Num RealNum where
  fromInteger = RInt
  (+) = liftBinary (+) (+) (+)
  (-) = liftBinary (-) (-) (-)
  (*) = liftBinary (*) (*) (*)
  abs = liftUnary abs abs abs
  signum = liftUnary signum signum signum
  negate = liftUnary negate negate negate

-- | Escape characters, with the letter after @\\@.
escapes :: [(Char, Char)]
escapes =
  [ ('\n', 'n'),
    ('\r', 'r'),
    ('\t', 't'),
    ('"', '"'),
    ('\\', '\\')
  ]

-- | Named characters, with their names.
charNames :: [(Char, String)]
charNames =
  [ (' ', "space"),
    ('\n', "newline")
  ]
