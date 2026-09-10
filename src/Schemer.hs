module Schemer where

import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (spaces)

data SExp
  = Atom String
  | Number Integer
  | String String
  | Bool Bool
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
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- Parse a decimal number (base 10) without any prefix.
parseDec :: Parser SExp
parseDec = Number . read <$> many1 digit

-- Parse a number with a base prefix (#b, #o, #d, #x) or a boolean (#t, #f).
parseHash :: Parser SExp
parseHash =
  char '#'
    >> ( (char 't' >> return (Bool True))
           <|> (char 'f' >> return (Bool False))
           <|> (char 'b' >> parseBin)
           <|> (char 'o' >> parseOct)
           <|> (char 'd' >> parseDec)
           <|> (char 'x' >> parseHex)
       )

stringToBaseInt :: Integer -> String -> Integer
stringToBaseInt base = foldl' (\acc c -> acc * base + toInteger (digitToInt c)) 0

parseBin :: Parser SExp
parseBin = Number . stringToBaseInt 2 <$> many1 (oneOf "01")

parseOct :: Parser SExp
parseOct = Number . stringToBaseInt 8 <$> many1 octDigit

parseHex :: Parser SExp
parseHex = Number . stringToBaseInt 16 <$> many1 hexDigit

parseExpr :: Parser SExp
parseExpr =
  parseAtom
    <|> parseString
    <|> parseDec
    <|> parseHash

-- Parsec's `spaces = skipMany space` allows zero spaces,
-- so we hide that and define our own here.
-- spaces :: Parser ()
-- spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
