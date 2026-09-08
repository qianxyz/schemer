module Main (main) where

import System.Environment
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
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

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
parseNumber = fmap (Number . read) $ many1 digit

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

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
