module Schemer where

import Data.Char (digitToInt, toLower)
import Data.Complex (Complex ((:+)))
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Vector (Vector, fromList)
import Text.Parsec
import Text.Parsec.String (Parser)

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

-- | Parse a string enclosed by @"@.
parseString :: Parser SExp
parseString = String <$> (between (char '"') (char '"') $ many charInString)

-- | Parse a single character in a string, handling escape sequences.
charInString :: Parser Char
charInString = noneOf "\\\"" <|> escapeSequence

-- | Parse escape sequences: @\\\"@, @\\\\@, @\\n@, @\\r@, @\\t@.
escapeSequence :: Parser Char
escapeSequence =
  char '\\'
    >> choice
      [ char '"',
        char '\\',
        char 'n' >> return '\n',
        char 'r' >> return '\r',
        char 't' >> return '\t'
      ]

-- | Parse an atom (also known as symbol or identifier).
--
-- Note it does not handle peculiar identifiers (@+@, @-@, @...@):
-- @+@ and @-@ are treated as a special case in parsing numbers,
-- and @...@ is not supported yet. -- TODO
--
-- From R5RS:
--
-- > <identifier> ::= <initial> <subsequent>* | <peculiar identifier>
-- > <initial>    ::= <letter> | ! $ % & * / : < = > ? ^ _ ~
-- > <subsequent> ::= <initial> | <digit> | + - . @
-- > <peculiar identifier> ::= + | - | ...
parseAtom :: Parser SExp
parseAtom = Atom <$> liftA2 (:) initial (many subsequent)
  where
    initial = letter <|> oneOf "!$%&*/:<=>?^_~"
    subsequent = initial <|> digit <|> oneOf "+-.@"

-- <complex R> ::=
--     <ureal R> [+/- [<ureal R>] i]
--   | +/- [<ureal R>] i
--   | +/-  <ureal R> [+/- [<ureal R>] i]
parseComplex :: Radix -> Parser SExp
parseComplex radix = unsigned <|> signed
  where
    unsigned = parseUReal radix >>= attachImgPart
    signed = do
      sign <- oneOf "+-"
      mureal <- optionMaybe (parseUReal radix)
      mi <- optionMaybe (char 'i')
      case (mureal, mi) of
        (Nothing, Nothing) -> return $ Atom [sign]
        (_, Just _) ->
          let img = applySign sign (fromMaybe 1 mureal)
           in return $ Complex (0 :+ img)
        (Just ureal, _) -> attachImgPart $ applySign sign ureal
    attachImgPart real = do
      mimg <- optionMaybe (parseImgPart radix)
      return $ case mimg of
        Nothing -> Real real
        Just img -> Complex (real :+ img)

applySign :: Char -> RealNum -> RealNum
applySign '-' = negate
applySign _ = id

-- Parse +/- [<ureal R>] i.
parseImgPart :: Radix -> Parser RealNum
parseImgPart radix = do
  sign <- oneOf "+-"
  ureal <- option 1 (parseUReal radix)
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

makeRational :: Integer -> Integer -> Parser RealNum
makeRational _ 0 = fail "Denominator cannot be zero"
makeRational num denom = return $ fromRatio (num % denom)

data Radix = B | O | D | X deriving (Show, Eq)

parseRadix :: Parser Radix
parseRadix =
  (char 'b' >> return B)
    <|> (char 'o' >> return O)
    <|> (char 'd' >> return D)
    <|> (char 'x' >> return X)

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
    <|> (parseRadix >>= parseComplex)
    <|> (char '\\' >> parseChar)
    <|> Vector . fromList <$> between (char '(') (char ')') parseExprs

parseExprs :: Parser [SExp]
parseExprs = sepEndBy parseExpr spaces1

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

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseList :: Parser SExp
parseList = do
  xs <- parseExprs
  if null xs
    then return $ List []
    else do
      mlast <- optionMaybe $ char '.' >> spaces1 >> parseExpr
      return $ maybe (List xs) (DottedList xs) mlast

desugarQuotes :: String -> Parser a -> Parser SExp
desugarQuotes s ps = do
  _ <- ps
  x <- parseExpr
  return $ List [Atom s, x]

parseQuoted :: Parser SExp
parseQuoted = desugarQuotes "quote" $ char '\''

parseQuasiquote :: Parser SExp
parseQuasiquote = desugarQuotes "quasiquote" $ char '`'

parseUnquote :: Parser SExp
parseUnquote =
  (desugarQuotes "unquote-splicing" $ string' ",@")
    <|> (desugarQuotes "unquote" $ char ',')

parseExpr :: Parser SExp
parseExpr =
  parseAtom
    <|> parseString
    <|> parseComplex D
    <|> parseHash
    <|> parseQuoted
    <|> parseQuasiquote
    <|> parseUnquote
    <|> (between (char '(') (char ')') parseList)

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
