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
    safeDiv,
    toComplexDouble,

    -- * Real numbers
    RealNum,
    pattern RInt,
    pattern RRational,
    pattern RFloat,
    ratio,

    -- * Lexical tables
    escapes,
    charNames,
  )
where

import Data.Complex (Complex ((:+)), magnitude)
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

-- | Convert a Scheme number to a Haskell @Complex Double@.
-- Used for inexact numeric operations.
toComplexDouble :: Number -> Complex Double
toComplexDouble (Real r) = toDouble r :+ 0
toComplexDouble (Complex (r :+ i)) = toDouble r :+ toDouble i

-- | Convert a Haskell @Complex Double@ to a Scheme number.
-- Used for inexact numeric operations.
fromComplexDouble :: Complex Double -> Number
fromComplexDouble (r :+ i) = Complex (RFloat r :+ RFloat i)

instance Num Number where
  fromInteger = Int

  (Real r1) + (Real r2) = Real (r1 + r2)
  n1 + n2 =
    let (r1 :+ i1) = toComplex n1
        (r2 :+ i2) = toComplex n2
     in Complex ((r1 + r2) :+ (i1 + i2))
  (Real r1) - (Real r2) = Real (r1 - r2)
  n1 - n2 =
    let (r1 :+ i1) = toComplex n1
        (r2 :+ i2) = toComplex n2
     in Complex ((r1 - r2) :+ (i1 - i2))
  (Real r1) * (Real r2) = Real (r1 * r2)
  n1 * n2 =
    let (r1 :+ i1) = toComplex n1
        (r2 :+ i2) = toComplex n2
     in Complex ((r1 * r2 - i1 * i2) :+ (r1 * i2 + r2 * i1))

  negate (Real r) = Real (negate r)
  negate (Complex (r :+ i)) = Complex (negate r :+ negate i)

  abs (Real r) = Real (abs r)
  -- abs and signum are always inexact for complex, even when they can
  -- be exact, e.g. 3+4i. This is allowed in R5RS.
  abs (Complex (r :+ i)) = Float $ magnitude (toDouble r :+ toDouble i)

  signum (Real r) = Real (signum r)
  signum n
    | m == 0 = Float 0
    | otherwise = fromComplexDouble $ dn / (m :+ 0)
    where
      dn = toComplexDouble n
      m = magnitude dn

safeDiv :: Number -> Number -> Maybe Number
safeDiv (Real r1) (Real r2) = Real <$> safeDivR r1 r2
safeDiv n1 (Real r2) = do
  let (r1 :+ i1) = toComplex n1
  r <- safeDivR r1 r2
  i <- safeDivR i1 r2
  return $ Complex (r :+ i)
safeDiv n1 n2@(Complex (r2 :+ i2)) =
  if isFloat r1 || isFloat i1 || isFloat r2 || isFloat i2
    then return . fromComplexDouble $ toComplexDouble n1 / toComplexDouble n2
    else do
      let mag2 = r2 * r2 + i2 * i2
      r <- safeDivR (r1 * r2 + i1 * i2) mag2
      i <- safeDivR (r2 * i1 - r1 * i2) mag2
      return $ Complex (r :+ i)
  where
    (r1 :+ i1) = toComplex n1

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

safeDivR :: RealNum -> RealNum -> Maybe RealNum
safeDivR a b
  | b == RInt 0 = Nothing
  | isFloat a || isFloat b = Just . RFloat $ toDouble a / toDouble b
  | otherwise = Just . RRational $ toRatio a / toRatio b

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
