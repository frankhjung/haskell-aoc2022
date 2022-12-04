module Main (main) where

import           Solve      (solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day03" $ do
    context "part 1" $
      it "expect sum of input to be 157" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 157
    context "part 2" $
      it "expect sum of input to be 70" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` 70
