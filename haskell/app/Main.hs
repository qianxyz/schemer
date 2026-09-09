module Main (main) where

import Schemer
import System.Environment

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
