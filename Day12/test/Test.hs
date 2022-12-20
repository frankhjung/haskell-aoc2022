module Main (main) where

import           Solve      (solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day12" $ do
    context "part 1" $
      it "expect 31 steps from S to E" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 31
    context "part 2" $
      it "expect unit" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` ()
