module Schemer.Eval where

import Control.Monad
import Schemer.Error
import Schemer.Types

eval :: SExp -> ThrowsError SExp
eval e@(Number _) = return e
eval e@(String _) = return e
eval e@(Bool _) = return e
eval e@(Char _) = return e
eval e@(Vector _) = return e
-- TODO: treat (quote a b) as malformed quote
eval (List [Atom "quote", e]) = return e
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
    ("+", primPlusMult (+) 0),
    ("-", primMinusDiv (\a b -> return $ a - b) 0),
    ("*", primPlusMult (*) 1),
    ("/", primMinusDiv (divThrow) 1),
    -- TODO: handle zero divisor for
    ("modulo", primIntPair mod),
    ("quotient", primIntPair quot),
    ("remainder", primIntPair rem),
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
    ("symbol->string", primSymbolToString),
    ("string->symbol", primStringToSymbol)
  ]

assertInt :: SExp -> ThrowsError Integer
assertInt (Number (Int a)) = return a
assertInt e = throwError $ TypeMismatch "int" e

assertNumber :: SExp -> ThrowsError Number
assertNumber (Number a) = return a
assertNumber e = throwError $ TypeMismatch "number" e

divThrow :: Number -> Number -> ThrowsError Number
divThrow a b = case safeDiv a b of
  Nothing -> throwError $ ZeroDivision
  Just x -> return x

primPlusMult ::
  (Number -> Number -> Number) -> Number -> [SExp] -> ThrowsError SExp
primPlusMult op zero exprs =
  mapM assertNumber exprs
    >>= return . Number . foldl op zero

primMinusDiv ::
  (Number -> Number -> ThrowsError Number) ->
  Number ->
  [SExp] ->
  ThrowsError SExp
primMinusDiv _ _ [] = throwError $ NumArgs ">= 1" []
primMinusDiv op zero [e] = assertNumber e >>= op zero >>= return . Number
primMinusDiv op _ (fe : exprs) = do
  fn <- assertNumber fe
  ns <- mapM assertNumber exprs
  res <- foldM op fn ns
  return $ Number res

-- primMinusDiv op _ exprs = Number . foldl1 op $ map assertNumber exprs

primIntPair :: (Integer -> Integer -> Integer) -> [SExp] -> ThrowsError SExp
primIntPair op [a, b] = do
  ia <- assertInt a
  ib <- assertInt b
  return . Number . Int $ op ia ib
primIntPair _ e = throwError $ NumArgs "2" e

-- | Convert a symbol to its name.
primSymbolToString :: [SExp] -> ThrowsError SExp
primSymbolToString [Atom name] = return $ String name
primSymbolToString [e] = throwError $ TypeMismatch "symbol" e
primSymbolToString e = throwError $ NumArgs "1" e

-- | Convert a string to the symbol with that name.
primStringToSymbol :: [SExp] -> ThrowsError SExp
primStringToSymbol [String name] = return $ Atom name
primStringToSymbol [e] = throwError $ TypeMismatch "string" e
primStringToSymbol e = throwError $ NumArgs "1" e

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
