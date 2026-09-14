module Schemer where

import Data.Char (digitToInt, toLower)
import Data.Complex (Complex ((:+)))
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import Text.ParserCombinators.Parsec hiding (spaces)

data SExp
  = Atom String
  | Real RealNum
  | Complex (Complex RealNum)
  | String String
  | Bool Bool
  | Char Char
  | List [SExp]
  | -- A dotted list like (a b . c) is a list that ends with c instead of nil.
    -- It can be constructed e.g. with `(cons a (cons b c))`.
    DottedList [SExp] SExp
  deriving (Show, Eq)

parseString :: Parser SExp
parseString = do
  _ <- char '"'
  x <- many stringChar
  _ <- char '"'
  return $ String x

-- Parse a single character in a string, handling escape sequences.
stringChar :: Parser Char
stringChar = noneOf "\\\"" <|> (char '\\' >> escapeSequence)

escapeSequence :: Parser Char
escapeSequence =
  char '"'
    <|> char '\\'
    <|> (char 'n' >> return '\n')
    <|> (char 'r' >> return '\r')
    <|> (char 't' >> return '\t')

parseAtom :: Parser SExp
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

symbol :: Parser Char
symbol = oneOf "!$%&|*/:<=>?@^_~"

-- <complex R> ::= [+/-] <ureal R> [+/- [<ureal R>] i]
--               |  +/- [<ureal R>] i
parseComplex :: Radix -> Parser SExp
parseComplex radix = do
  sign <- optionMaybe (oneOf "+-")
  realPart <- optionMaybe (parseUReal radix)
  mi <- optionMaybe (char 'i')
  case (sign, realPart, mi) of
    -- Special case: Parse +/- sign as an atom.
    (Just s, Nothing, Nothing) -> return $ Atom [s]
    -- Imaginary part only: +/- [<ureal R>] i.
    (Just s, _, Just _) ->
      let uimg = fromMaybe 1 realPart
       in return $ Complex (0 :+ applySign s uimg)
    -- Got real part [+/-] <ureal R>, left to parse img part
    (_, Just ureal, Nothing) -> do
      let real = maybe id applySign sign ureal
      imgPart <- optionMaybe (parseImgPart radix)
      case imgPart of
        Nothing -> return $ Real real
        Just img -> return $ Complex (real :+ img)
    _ -> fail "Invalid number format"

applySign :: Char -> RealNum -> RealNum
applySign c = if c == '-' then negate else id

-- Parse +/- [<ureal R>] i.
parseImgPart :: Radix -> Parser RealNum
parseImgPart radix = do
  sign <- oneOf "+-"
  ureal <- option (Int 1) (parseUReal radix)
  _ <- char 'i'
  return $ applySign sign ureal

data RealNum
  = Int Integer
  | Rational Rational
  | Float Double
  deriving (Show, Eq)

-- TODO: Implement RealFloat for RealNum

isFloat :: RealNum -> Bool
isFloat (Float _) = True
isFloat _ = False

-- Build a rational, demoting to Int when the denominator is 1.
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

-- Lift unary operations on Integer, Rational, and Double to RealNum.
liftUnary ::
  (Integer -> Integer) ->
  (Rational -> Rational) ->
  (Double -> Double) ->
  RealNum ->
  RealNum
liftUnary fi _ _ (Int a) = Int (fi a)
liftUnary _ fr _ (Rational a) = fromRatio (fr a)
liftUnary _ _ fd (Float a) = Float (fd a)

-- Lift binary operations on Integer, Rational, and Double to RealNum.
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

  -- int / int should produce a rational
  a / b
    | isFloat a || isFloat b = Float (toDouble a / toDouble b)
    | otherwise = fromRatio (toRatio a / toRatio b)

-- Parse <uint R>[/<uint R>]. If R = D also parse <uint R>.<uint R>.
parseUReal :: Radix -> Parser RealNum
parseUReal radix = do
  first <- parseDigitString radix
  msep <- optionMaybe $ if radix == D then oneOf "/." else char '/'
  let toInt = stringToBaseInt radix
  case msep of
    Nothing -> return $ Int (toInt first)
    Just sep -> do
      second <- parseDigitString radix
      case sep of
        '.' -> return $ Float (read (first ++ "." ++ second))
        _ -> makeRational (toInt first) (toInt second)
      where
        makeRational _ 0 = fail "Denominator cannot be zero"
        makeRational num denom = return $ fromRatio (num % denom)

data Radix = B | O | D | X deriving (Show, Eq)

parseDigit :: Radix -> Parser Char
parseDigit B = oneOf "01"
parseDigit O = octDigit
parseDigit D = digit
parseDigit X = hexDigit

radixToBase :: Radix -> Integer
radixToBase B = 2
radixToBase O = 8
radixToBase D = 10
radixToBase X = 16

stringToBaseInt :: Radix -> String -> Integer
stringToBaseInt D = read
stringToBaseInt radix =
  foldl' (\acc c -> acc * radixToBase radix + toInteger (digitToInt c)) 0

parseDigitString :: Radix -> Parser String
parseDigitString = many1 . parseDigit

-- Parse a #-prefixed expression.
parseHash :: Parser SExp
parseHash = do
  _ <- char '#'
  (char 't' >> return (Bool True))
    <|> (char 'f' >> return (Bool False))
    <|> (char 'b' >> parseComplex B)
    <|> (char 'o' >> parseComplex O)
    <|> (char 'd' >> parseComplex D)
    <|> (char 'x' >> parseComplex X)
    <|> (char '\\' >> parseChar)

parseChar :: Parser SExp
parseChar = Char <$> (namedOrSingleLetter <|> anyChar)

namedOrSingleLetter :: Parser Char
namedOrSingleLetter = do
  str <- many1 letter
  case str of
    [c] -> return c
    _ -> case map toLower str of
      "space" -> return ' '
      "newline" -> return '\n'
      _ -> fail $ "Unknown character literal: " ++ str

parseExpr :: Parser SExp
parseExpr =
  parseAtom
    <|> parseString
    <|> parseComplex D
    <|> parseHash

-- Parsec's `spaces = skipMany space` allows zero spaces,
-- so we hide that and define our own here.
-- spaces :: Parser ()
-- spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
