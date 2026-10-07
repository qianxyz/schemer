module Schemer
  ( module Schemer.Parse,
    module Schemer.Types,
    run,
  )
where

import Data.Text (unpack)
import Data.Text.Display (display)
import Schemer.Eval
import Schemer.Parse
import Schemer.Types

run :: String -> Either String String
run source = case readExpr source >>= eval of
  Left err -> Left . unpack . display $ err
  Right expr -> Right . unpack . display $ expr
