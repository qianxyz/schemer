module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "placeholder" $ do
    it "runs" $ do
      (1 + 1 :: Int) `shouldBe` 2
