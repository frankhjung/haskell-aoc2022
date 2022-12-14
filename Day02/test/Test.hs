module Main (main) where

import           Solve      (solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day02" $ do
    context "part 1" $
      it "expect sum of input to be 15" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 15
    context "part 2" $
      it "expect sum of input to be 12" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` 12
