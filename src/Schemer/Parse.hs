module Schemer.Parse
  ( readExpr,
    parseExpr,
  )
where

import Data.Char (digitToInt, toLower, toUpper)
import Data.Complex (Complex ((:+)))
import Data.Vector (fromList)
import Schemer.Types
import Text.Parsec
import Text.Parsec.String (Parser)

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
    >> choice [escaped <$ char escaping | (escaped, escaping) <- escapes]

-- | Parse an atom (also known as symbol or identifier). From R5RS:
--
-- > <identifier> ::= <initial> <subsequent>* | <peculiar identifier>
-- > <initial>    ::= <letter> | ! $ % & * / : < = > ? ^ _ ~
-- > <subsequent> ::= <initial> | <digit> | + - . @
-- > <peculiar identifier> ::= + | - | ...
parseAtom :: Parser SExp
parseAtom =
  Atom
    <$> ( (:) <$> initial <*> (many subsequent)
            -- Need `endOfToken` check here to not parse +/-<number>.
            <|> try (peculiarIdentifier <* endOfToken)
        )
  where
    initial = letter <|> oneOf "!$%&*/:<=>?^_~"
    subsequent = initial <|> digit <|> oneOf "+-.@"
    peculiarIdentifier = choice $ string' <$> ["+", "-", "..."]

-- | Succeed without consuming input if the current token has ended,
-- i.e. the next character is a delimiter or EOF.
endOfToken :: Parser ()
endOfToken = notFollowedBy $ noneOf " \n\t\r()\";"

-- | Parse a (complex) number under a radix.
--
-- > <complex>  ::= <ureal> [<imagPart>]
-- >              | +/- i
-- >              | +/- <ureal> i
-- >              | +/- <ureal> [<imagPart>]
-- > <imagPart> ::= +/- [<ureal>] i
parseNumber :: Radix -> Parser SExp
parseNumber radix = Number <$> (unsigned <|> signed)
  where
    unsigned = parseUReal radix >>= attachImagPart -- <ureal> [<imagPart>]
    signed = do
      sign <- oneOf "+-"
      (pureImag $ applySign sign 1) -- +/- i
        <|> do
          ureal <- parseUReal radix
          let real = applySign sign ureal
          pureImag real -- +/- <ureal> i
            <|> attachImagPart real -- +/- <ureal> [<imagPart>]
    pureImag i = char 'i' >> return (Complex $ 0 :+ i)
    attachImagPart real = do
      mimag <- optionMaybe (parseImagPart radix)
      return $ case mimag of
        Nothing -> Real real
        Just imag -> Complex (real :+ imag)

applySign :: Char -> RealNum -> RealNum
applySign '-' = negate
applySign _ = id

-- | Parse @<imagPart> ::= +/- [<ureal>] i@.
parseImagPart :: Radix -> Parser RealNum
parseImagPart radix =
  applySign <$> oneOf "+-" <*> option 1 (parseUReal radix) <* char 'i'

-- | Parse an unsigned real number.
--
-- > <ureal R>    ::= <uint R>
-- >                | <uint R> / <uint R>
-- >                | <decimal R>
-- > <decimal 10> ::= <digit>+ . <digit>+
--
-- Note that decimals are only defined for base 10,
-- and both integer and fractional part should be present.
parseUReal :: Radix -> Parser RealNum
parseUReal radix = do
  first <- parseDigits radix
  msep <- optionMaybe $ if radix == Dec then oneOf "/." else char '/'
  let toInt = digitsToInteger radix
  case msep of
    Nothing -> return $ RInt (toInt first)
    Just sep -> do
      second <- parseDigits radix
      case sep of
        '.' -> return $ RFloat (read (first ++ "." ++ second))
        _ ->
          maybe (fail "Denominator cannot be 0") return $
            ratio (toInt first) (toInt second)

-- | The radix of a number, indicated by a prefix of
-- @#b@, @#o@, @#d@ or @#x@.
data Radix = Bin | Oct | Dec | Hex deriving (Show, Eq)

-- | Parse the character after @#@ into its corresponding radix.
parseRadix :: Parser Radix
parseRadix =
  (char 'b' >> return Bin)
    <|> (char 'o' >> return Oct)
    <|> (char 'd' >> return Dec)
    <|> (char 'x' >> return Hex)

-- | Parse a digit in a specific radix.
parseDigit :: Radix -> Parser Char
parseDigit Bin = oneOf "01"
parseDigit Oct = octDigit
parseDigit Dec = digit
parseDigit Hex = hexDigit

-- | Convert a radix into its base as a number.
radixToBase :: Radix -> Integer
radixToBase Bin = 2
radixToBase Oct = 8
radixToBase Dec = 10
radixToBase Hex = 16

-- | Convert a string of digits into an integer in a specific radix.
digitsToInteger :: Radix -> String -> Integer
digitsToInteger Dec = read
digitsToInteger r =
  foldl' (\acc c -> acc * radixToBase r + toInteger (digitToInt c)) 0

-- | Parse a string of digits in a specific radix.
parseDigits :: Radix -> Parser String
parseDigits = many1 . parseDigit

-- | Parse an expression prefixed by @#@, which can be:
--
-- * a boolean (@#t@ or @#f@);
-- * a number prefixed by @#b@, @#o@, @#d@, @#x@;
-- * a character literal, like @#\\a@ or @#\\space@;
-- * a vector, like @#(a b c)@.
parseHash :: Parser SExp
parseHash = do
  _ <- char '#'
  (char 't' >> return (Bool True))
    <|> (char 'f' >> return (Bool False))
    <|> (parseRadix >>= parseNumber)
    <|> (char '\\' >> parseChar)
    <|> Vector
      . fromList
      <$> parens parseExprs

-- | @parens p@ parses @p@ enclosed between @(@ and @)@.
-- Spaces are allowed after @(@ and before @)@.
parens :: Parser a -> Parser a
parens = between (char '(' >> spaces) (spaces >> char ')')

-- | Parse a list of expressions, separated and optionally ended by spaces.
parseExprs :: Parser [SExp]
parseExprs = sepEndBy parseExpr spaces1

-- | Parse a Scheme character literal after @#\\@, which can be
-- a character name (@space@ or @newline@) or a single character.
-- Must be followed by a delimiter.
parseChar :: Parser SExp
parseChar = Char <$> ((tryCharNames <|> anyChar) <* endOfToken)
  where
    tryCharNames = choice [c <$ stringCI' name | (c, name) <- charNames]

-- | Parse a case-insensitive string, without consuming matching prefix.
stringCI' :: String -> Parser String
stringCI' = try . traverse (\c -> oneOf [toLower c, toUpper c])

-- | Parse one or more spaces.
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- | Parse a list or a dotted list, including the enclosing @()@.
-- Spaces are allowed after @(@ and before @)@.
parseListOrDotted :: Parser SExp
parseListOrDotted = parens listBody

-- | Parse the body of a list between @()@ (not including them).
listBody :: Parser SExp
listBody = do
  xs <- parseExprs
  if null xs -- dotted list must have something before dot
    then return $ List []
    else do
      mlast <- optionMaybe $ char '.' >> spaces1 >> parseExpr
      return $ maybe (List xs) (DottedList xs) mlast

-- | @quoteForm name@ parses an expression @e@ and return
-- the Scheme list @(name e)@. Used to construct list forms
-- from the quote forms.
quoteForm :: String -> Parser SExp
quoteForm name = do
  x <- parseExpr
  return $ List [Atom name, x]

-- | Parse a quoted expression @'e@, returning @(quote e)@.
parseQuoted :: Parser SExp
parseQuoted = char '\'' >> quoteForm "quote"

-- | Parse a quasiquoted expression @\`e@, returning @(quasiquote e)@.
parseQuasiquoted :: Parser SExp
parseQuasiquoted = char '`' >> quoteForm "quasiquote"

-- | Parse an unquoted expression
--
-- * @,\@e@ into @(unquote-splicing e)@;
-- * @,e@ into @(unquote e)@.
parseUnquoted :: Parser SExp
parseUnquoted =
  -- Must try ,@ first since , is a prefix of it.
  (string' ",@" >> quoteForm "unquote-splicing")
    <|> (char ',' >> quoteForm "unquote")

parseExpr :: Parser SExp
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber Dec
    <|> parseHash
    <|> parseQuoted
    <|> parseQuasiquoted
    <|> parseUnquoted
    <|> parseListOrDotted

readExpr :: String -> Either String SExp
readExpr input = case parse parseExpr "scheme" input of
  Left err -> Left $ show err
  Right val -> Right val
