module Main (main) where

import Data.Text.IO qualified as T (putStrLn)
import Schemer
import System.Environment

main :: IO ()
main = do
  (expr : _) <- getArgs
  case readExpr expr of
    Left err -> putStrLn $ "Error: " ++ err
    Right valText -> T.putStrLn valText
