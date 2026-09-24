module Schemer.Eval where

import Data.Maybe (fromMaybe)
import Schemer.Types

eval :: SExp -> SExp
eval e@(Number _) = e
eval e@(String _) = e
eval e@(Bool _) = e
eval e@(Char _) = e
eval (List [Atom "quote", e]) = e
eval (List (Atom func : args)) = apply func $ map eval args
eval (Atom _) = undefined -- TODO
eval (List _) = undefined -- TODO
eval (DottedList _ _) = undefined -- TODO
eval (Vector _) = undefined -- TODO

apply :: String -> [SExp] -> SExp
apply func args = maybe (undefined) ($ args) $ lookup func primitives

primitives :: [(String, [SExp] -> SExp)]
primitives =
  [ ("+", primPlusMult (+) 0),
    ("-", primMinusDiv (-) 0),
    ("*", primPlusMult (*) 1),
    ("/", primMinusDiv (unsafeDiv) 1),
    ("modulo", Number . Int . uncurry mod . assertIntPair),
    ("quotient", Number . Int . uncurry quot . assertIntPair),
    ("remainder", Number . Int . uncurry rem . assertIntPair)
  ]

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
