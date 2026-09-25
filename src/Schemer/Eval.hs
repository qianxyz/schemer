module Schemer.Eval where

import Data.Maybe (fromMaybe)
import Schemer.Types

eval :: SExp -> SExp
eval e@(Number _) = e
eval e@(String _) = e
eval e@(Bool _) = e
eval e@(Char _) = e
eval e@(Vector _) = e
eval (List [Atom "quote", e]) = e
eval (List (Atom func : args)) = apply func $ map eval args
eval (Atom _) = undefined -- TODO
eval (List _) = undefined -- TODO
eval (DottedList _ _) = undefined -- TODO

apply :: String -> [SExp] -> SExp
apply func args = maybe (undefined) ($ args) $ lookup func primitives

primitives :: [(String, [SExp] -> SExp)]
primitives =
  [ -- numerical operations
    ("+", primPlusMult (+) 0),
    ("-", primMinusDiv (-) 0),
    ("*", primPlusMult (*) 1),
    ("/", primMinusDiv (unsafeDiv) 1),
    ("modulo", Number . Int . uncurry mod . assertIntPair),
    ("quotient", Number . Int . uncurry quot . assertIntPair),
    ("remainder", Number . Int . uncurry rem . assertIntPair),
    -- type-testing
    ("boolean?", typePredicate isBoolean),
    ("symbol?", typePredicate isSymbol),
    ("char?", typePredicate isChar),
    ("vector?", typePredicate isVector),
    ("pair?", typePredicate isPair),
    ("number?", typePredicate isNumber),
    ("string?", typePredicate isString),
    ("null?", typePredicate isNull),
    ("symbol->string", primSymbolToString),
    ("string->symbol", primStringToSymbol)
  ]

-- | Convert a symbol to its name.
primSymbolToString :: [SExp] -> SExp
primSymbolToString [Atom name] = String name
primSymbolToString _ = undefined

-- | Convert a string to the symbol with that name.
primStringToSymbol :: [SExp] -> SExp
primStringToSymbol [String name] = Atom name
primStringToSymbol _ = undefined

-- | Lift a predicate on one expression to a primitive taking one argument.
typePredicate :: (SExp -> Bool) -> [SExp] -> SExp
typePredicate p [e] = Bool (p e)
typePredicate _ _ = undefined

isBoolean, isSymbol, isChar, isVector, isPair, isNumber, isString, isNull :: SExp -> Bool
isBoolean (Bool _) = True
isBoolean _ = False
isSymbol (Atom _) = True
isSymbol _ = False
isChar (Char _) = True
isChar _ = False
isVector (Vector _) = True
isVector _ = False
-- A pair is any non-empty list, proper or dotted.
isPair (List (_ : _)) = True
isPair (DottedList _ _) = True
isPair _ = False
isNumber (Number _) = True
isNumber _ = False
isString (String _) = True
isString _ = False
-- Only the empty list is null.
isNull (List []) = True
isNull _ = False

unsafeDiv :: Number -> Number -> Number
unsafeDiv a b = fromMaybe undefined $ safeDiv a b

assertNumber :: SExp -> Number
assertNumber (Number n) = n
assertNumber _ = undefined

assertIntPair :: [SExp] -> (Integer, Integer)
assertIntPair [Number (Int n), Number (Int m)] = (n, m)
assertIntPair _ = undefined

primPlusMult :: (Number -> Number -> Number) -> Number -> [SExp] -> SExp
primPlusMult op zero = Number . foldl op zero . map assertNumber

primMinusDiv :: (Number -> Number -> Number) -> Number -> [SExp] -> SExp
primMinusDiv _ _ [] = undefined
primMinusDiv op zero [e] = Number . op zero $ assertNumber e
primMinusDiv op _ exprs = Number . foldl1 op $ map assertNumber exprs
