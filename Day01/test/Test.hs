module Main (main) where

import           Solve      (solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day01" $ do
    context "part 1" $
      it "most calories carried by an elf is 24000" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 24000
    context "part 2" $
      it "calories carried by top 3 elves is 45000" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` 45000
