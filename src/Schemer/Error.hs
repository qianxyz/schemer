{-# LANGUAGE OverloadedStrings #-}

module Schemer.Error where

import Data.Text.Display (Display (displayBuilder, displayList))
import Schemer.Types
import Text.Parsec (ParseError)

data SchemeError
  = ParseError ParseError
  | ZeroDivision
  | NumArgs String [SExp]
  | TypeMismatch String SExp
  | NotFunction String String
  | BadSpecialForm String SExp
  deriving (Show, Eq)

instance Display SchemeError where
  displayBuilder (ParseError e) = displayBuilder $ show e
  displayBuilder ZeroDivision = "Division by zero"
  displayBuilder (NumArgs expected found) =
    "Invalid number of args: expected "
      <> displayBuilder expected
      <> ", found values "
      <> displayList found
  displayBuilder (TypeMismatch expected found) =
    "Invalid type: expected "
      <> displayBuilder expected
      <> ", found "
      <> displayBuilder found
  displayBuilder (NotFunction message func) =
    displayBuilder message <> ": " <> displayBuilder func
  displayBuilder (BadSpecialForm message form) =
    displayBuilder message <> ": " <> displayBuilder form

type ThrowsError = Either SchemeError

throwError :: SchemeError -> ThrowsError a
throwError = Left
