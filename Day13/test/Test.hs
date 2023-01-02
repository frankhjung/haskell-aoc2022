module Main (main) where

import           Solve      (solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day13" $ do
    context "part 1" $
      it "expect 13" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 13
    context "part 2" $
      it "expect unit" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` ()
