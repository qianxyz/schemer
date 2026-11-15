module Schemer.Eval where

import Control.Monad
import Data.Char (toLower)
import Data.Function (on)
import Data.Functor.Classes (liftEq)
import Data.List (genericLength, genericReplicate)
import Schemer.Error
import Schemer.Types

eval :: SExp -> ThrowsError SExp
eval e@(Number _) = return e
eval e@(String _) = return e
eval e@(Bool _) = return e
eval e@(Char _) = return e
eval e@(Vector _) = return e
-- quote
eval (List [Atom "quote", e]) = return e
eval e@(List (Atom "quote" : _)) =
  throwError $ BadSpecialForm "Malformed quote form" e
-- if
eval (List [Atom "if", predicate, conseq, alt]) = do
  result <- eval predicate
  eval $ if isTruthy result then conseq else alt
eval (List [Atom "if", predicate, conseq]) =
  eval (List [Atom "if", predicate, conseq, unspecified])
eval e@(List (Atom "if" : _)) =
  throwError $ BadSpecialForm "Malformed if form" e
-- cond
eval (List (Atom "cond" : clauses@(_ : _))) = evalCond clauses
eval e@(List [Atom "cond"]) =
  throwError $ BadSpecialForm "Malformed cond form" e
-- case
eval (List (Atom "case" : key : clauses@(_ : _))) =
  eval key >>= \k -> evalCase k clauses
eval e@(List (Atom "case" : _)) =
  throwError $ BadSpecialForm "Malformed case form" e
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval e = throwError $ BadSpecialForm "Unrecognized special form" e

-- | Used when the value of an expression is unspecified in R5RS.
unspecified :: SExp
unspecified = Bool False

-- | Every value except #f counts as true.
isTruthy :: SExp -> Bool
isTruthy (Bool False) = False
isTruthy _ = True

-- | Evaluate a non-empty sequence of expressions in order and return
-- the value of the last one.
evalSequence :: SExp -> [SExp] -> ThrowsError SExp
evalSequence e es = eval e >>= \v -> foldM (\_ x -> eval x) v es

-- | Try cond clauses in order.
evalCond :: [SExp] -> ThrowsError SExp
evalCond [] = return unspecified
evalCond [List (Atom "else" : e : es)] = evalSequence e es
evalCond (clause@(List (Atom "else" : _)) : _) =
  throwError $
    BadSpecialForm
      "Malformed cond clause: else must be last and non-empty"
      clause
evalCond (clause@(List (test : body)) : rest) = do
  v <- eval test
  if not (isTruthy v)
    then evalCond rest
    else case body of
      [] -> return v -- (<test>) returns the value of the test
      [Atom "=>", _] -> undefined -- TODO: support (<test> => <expression>)
      (Atom "=>" : _) ->
        throwError $
          BadSpecialForm
            "Malformed cond clause: => expects a single expression"
            clause
      (e : es) -> evalSequence e es
evalCond (clause : _) =
  throwError $ BadSpecialForm "Malformed cond clause" clause

-- | Try case clauses in order, comparing the key with each datum by eqv?.
evalCase :: SExp -> [SExp] -> ThrowsError SExp
evalCase _ [] = return unspecified
evalCase _ [List (Atom "else" : e : es)] = evalSequence e es
evalCase key (List (List data' : e : es) : rest)
  | any (eqv key) data' = evalSequence e es
  | otherwise = evalCase key rest
evalCase _ (clause : _) =
  throwError $ BadSpecialForm "Malformed case clause" clause

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
    ("string->symbol", stringToSymbol),
    -- strings
    ("make-string", makeString),
    ("string", string),
    ("string-length", stringLength),
    ("string-ref", stringRef),
    -- pairs and lists
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    -- equivalence predicates
    ("eq?", binaryPredicate eqv),
    ("eqv?", binaryPredicate eqv),
    ("equal?", binaryPredicate equal)
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

expectChar :: SExp -> ThrowsError Char
expectChar (Char c) = return c
expectChar e = throwError $ TypeMismatch "char" e

-- | An exact non-negative integer, such as a length.
expectNatural :: SExp -> ThrowsError Integer
expectNatural e = do
  n <- expectInt e
  when (n < 0) $ throwError $ TypeMismatch "non-negative integer" e
  return n

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

car :: [SExp] -> ThrowsError SExp
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs "1" badArgList

cdr :: [SExp] -> ThrowsError SExp
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs "1" badArgList

cons :: [SExp] -> ThrowsError SExp
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs "2" badArgList

-- | Lift a predicate on two expressions to a primitive taking two arguments.
binaryPredicate :: (SExp -> SExp -> Bool) -> [SExp] -> ThrowsError SExp
binaryPredicate p [a, b] = return $ Bool (p a b)
binaryPredicate _ e = throwError $ NumArgs "2" e

-- | Used both for @eqv?@ and @eq?@ as R5RS allows them to be the same.
eqv :: SExp -> SExp -> Bool
eqv (Atom a) (Atom b) = a == b
eqv (Bool a) (Bool b) = a == b
eqv (Char a) (Char b) = a == b
eqv (Number a) (Number b) = a == b -- requires matching exactness
eqv (List []) (List []) = True
-- TODO: when we have identity for objects via IORefs
-- we can compare references for pairs, vectors and strings
eqv _ _ = False

-- | @equal?@: recursively compares structures with 'eqv'.
equal :: SExp -> SExp -> Bool
equal (List xs) (List ys) = liftEq equal xs ys
equal (DottedList xs x) (DottedList ys y) = liftEq equal xs ys && equal x y
equal (Vector xs) (Vector ys) = liftEq equal xs ys
equal (String a) (String b) = a == b
equal a b = eqv a b

-- | @(make-string k [char])@: a string of length k. Its contents are
-- unspecified without a char; NUL characters are used.
makeString :: [SExp] -> ThrowsError SExp
makeString [k] = makeString [k, Char '\0']
makeString [k, c] = do
  n <- expectNatural k
  ch <- expectChar c
  return . String $ genericReplicate n ch
makeString e = throwError $ NumArgs "1 or 2" e

-- | @(string char ...)@: a string of the given characters.
string :: [SExp] -> ThrowsError SExp
string args = String <$> traverse expectChar args

stringLength :: [SExp] -> ThrowsError SExp
stringLength [s] = Number . Int . genericLength <$> expectString s
stringLength e = throwError $ NumArgs "1" e

-- | @(string-ref string k)@: the character at zero-based index k.
stringRef :: [SExp] -> ThrowsError SExp
stringRef [s, k] = do
  str <- expectString s
  i <- expectInt k
  when (i < 0 || i >= genericLength str) $ throwError $ IndexOutOfRange i s
  return . Char $ str !! fromInteger i
stringRef e = throwError $ NumArgs "2" e
