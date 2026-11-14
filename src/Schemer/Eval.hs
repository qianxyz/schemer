module Schemer.Eval where

import Control.Monad
import Data.Char (toLower)
import Data.Function (on)
import Schemer.Error
import Schemer.Types

eval :: SExp -> ThrowsError SExp
eval e@(Number _) = return e
eval e@(String _) = return e
eval e@(Bool _) = return e
eval e@(Char _) = return e
eval e@(Vector _) = return e
eval (List [Atom "quote", e]) = return e
eval e@(List (Atom "quote" : _)) =
  throwError $ BadSpecialForm "Malformed quote form" e
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval e = throwError $ BadSpecialForm "Unrecognized special form" e

apply :: String -> [SExp] -> ThrowsError SExp
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    $ lookup func primitives

primitives :: [(String, [SExp] -> ThrowsError SExp)]
primitives =
  [ -- numerical operations
    ("+", numeric $ pure . sum),
    ("-", numeric subtractAll),
    ("*", numeric $ pure . product),
    ("/", numeric divideAll),
    ("modulo", integerDivision mod),
    ("quotient", integerDivision quot),
    ("remainder", integerDivision rem),
    -- numerical comparisons
    ("=", chain expectNumber numEq),
    ("<", chain expectReal $ ordered (== LT)),
    (">", chain expectReal $ ordered (== GT)),
    ("<=", chain expectReal $ ordered (/= GT)),
    (">=", chain expectReal $ ordered (/= LT)),
    -- string comparisons
    ("string=?", chain expectString (==)),
    ("string<?", chain expectString (<)),
    ("string>?", chain expectString (>)),
    ("string<=?", chain expectString (<=)),
    ("string>=?", chain expectString (>=)),
    ("string-ci=?", chain expectString $ caseInsensitive (==)),
    ("string-ci<?", chain expectString $ caseInsensitive (<)),
    ("string-ci>?", chain expectString $ caseInsensitive (>)),
    ("string-ci<=?", chain expectString $ caseInsensitive (<=)),
    ("string-ci>=?", chain expectString $ caseInsensitive (>=)),
    -- type-testing
    ("boolean?", typePredicate isBoolean),
    ("symbol?", typePredicate isSymbol),
    ("char?", typePredicate isChar),
    ("vector?", typePredicate isVector),
    ("pair?", typePredicate isPair),
    ("number?", typePredicate isNumber),
    ("string?", typePredicate isString),
    ("null?", typePredicate isNull),
    -- symbol handling
    ("symbol->string", symbolToString),
    ("string->symbol", stringToSymbol)
  ]

expectInt :: SExp -> ThrowsError Integer
expectInt (Number (Int a)) = return a
expectInt e = throwError $ TypeMismatch "int" e

expectNumber :: SExp -> ThrowsError Number
expectNumber (Number a) = return a
expectNumber e = throwError $ TypeMismatch "number" e

expectReal :: SExp -> ThrowsError RealNum
expectReal (Number (Real r)) = return r
expectReal e = throwError $ TypeMismatch "real" e

expectString :: SExp -> ThrowsError String
expectString (String s) = return s
expectString e = throwError $ TypeMismatch "string" e

-- | Lift an operation on numbers to one on S-expressions.
numeric :: ([Number] -> ThrowsError Number) -> [SExp] -> ThrowsError SExp
numeric f args = Number <$> (traverse expectNumber args >>= f)

-- | (- z) negates; (- z1 z2 ...) subtracts left to right.
subtractAll :: [Number] -> ThrowsError Number
subtractAll [] = throwError $ NumArgs ">= 1" []
subtractAll [n] = return $ negate n
subtractAll (n : ns) = return $ foldl (-) n ns

-- | (/ z) takes the reciprocal; (/ z1 z2 ...) divides left to right.
divideAll :: [Number] -> ThrowsError Number
divideAll [] = throwError $ NumArgs ">= 1" []
divideAll [n] = divide 1 n
divideAll (n : ns) = foldM divide n ns

divide :: Number -> Number -> ThrowsError Number
divide a b = maybe (throwError ZeroDivision) pure (safeDiv a b)

-- | A binary integer operation that rejects a zero divisor.
integerDivision ::
  (Integer -> Integer -> Integer) -> [SExp] -> ThrowsError SExp
integerDivision op [a, b] = do
  ia <- expectInt a
  ib <- expectInt b
  when (ib == 0) $ throwError ZeroDivision
  return . Number . Int $ op ia ib
integerDivision _ e = throwError $ NumArgs "2" e

-- | Check that a relation holds between each adjacent pair of
-- arguments: (< a b c) means a < b and b < c.
chain ::
  (SExp -> ThrowsError a) -> (a -> a -> Bool) -> [SExp] -> ThrowsError SExp
chain expect rel args
  | length args < 2 = throwError $ NumArgs ">= 2" args
  | otherwise = do
      xs <- traverse expect args
      return . Bool . and $ zipWith rel xs (drop 1 xs)

ordered :: (Ordering -> Bool) -> RealNum -> RealNum -> Bool
ordered ok a b = ok (compareReal a b)

-- | Compare strings after folding them to lower case.
caseInsensitive :: (String -> String -> Bool) -> String -> String -> Bool
caseInsensitive rel = rel `on` map toLower

-- | Convert a symbol to its name.
symbolToString :: [SExp] -> ThrowsError SExp
symbolToString [Atom name] = return $ String name
symbolToString [e] = throwError $ TypeMismatch "symbol" e
symbolToString e = throwError $ NumArgs "1" e

-- | Convert a string to the symbol with that name.
stringToSymbol :: [SExp] -> ThrowsError SExp
stringToSymbol [String name] = return $ Atom name
stringToSymbol [e] = throwError $ TypeMismatch "string" e
stringToSymbol e = throwError $ NumArgs "1" e

-- | Lift a predicate on one expression to a primitive taking one argument.
typePredicate :: (SExp -> Bool) -> [SExp] -> ThrowsError SExp
typePredicate p [e] = return $ Bool (p e)
typePredicate _ e = throwError $ NumArgs "1" e

isBoolean :: SExp -> Bool
isBoolean (Bool _) = True
isBoolean _ = False

isSymbol :: SExp -> Bool
isSymbol (Atom _) = True
isSymbol _ = False

isChar :: SExp -> Bool
isChar (Char _) = True
isChar _ = False

isVector :: SExp -> Bool
isVector (Vector _) = True
isVector _ = False

-- | A pair is any non-empty list, proper or dotted.
isPair :: SExp -> Bool
isPair (List (_ : _)) = True
isPair (DottedList _ _) = True
isPair _ = False

isNumber :: SExp -> Bool
isNumber (Number _) = True
isNumber _ = False

isString :: SExp -> Bool
isString (String _) = True
isString _ = False

-- | Only the empty list is null.
isNull :: SExp -> Bool
isNull (List []) = True
isNull _ = False
