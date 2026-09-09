module Schemer (SExp, parseExpr, readExpr) where

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
  deriving (Show)

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
    -- (<$) :: a -> f b -> f a
    -- (<$) = fmap . const  -- i.e. x <$ fb = fmap (\_ -> x) fb
    <|> ('\n' <$ char 'n')
    <|> ('\r' <$ char 'r')
    <|> ('\t' <$ char 't')

parseAtom :: Parser SExp
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseNumber :: Parser SExp
-- could also be written as:
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = fmap (Number . read) $ many1 digit
-- parseNumber = do
--   numStr <- many1 digit
--   return $ Number (read numStr)
-- parseNumber = many1 digit >>= \numStr -> return $ Number (read numStr)
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser SExp
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber

-- Parsec's `spaces = skipMany space` allows zero spaces,
-- so we hide that and define our own here.
-- spaces :: Parser ()
-- spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
