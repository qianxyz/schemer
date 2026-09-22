module Schemer.Eval where

import Schemer.Types

eval :: SExp -> SExp
eval e@(Number _) = e
eval e@(String _) = e
eval e@(Bool _) = e
eval e@(Char _) = e
eval (List [Atom "quote", e]) = e
eval (Atom _) = undefined -- TODO
eval (List _) = undefined -- TODO
eval (DottedList _ _) = undefined -- TODO
eval (Vector _) = undefined -- TODO
