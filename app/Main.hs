module Main (main) where

import Schemer
import System.Environment

main :: IO ()
main = do
  (expr : _) <- getArgs
  case readExpr expr of
    Left err -> putStrLn $ "Error: " ++ err
    Right val -> putStrLn $ "OK: " ++ val
