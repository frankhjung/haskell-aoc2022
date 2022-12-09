module Main (main) where

import           Data.Array (Array, listArray)
import           Solve      (countPerimeter, isBot, isLeft, isRight, isTop,
                             solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

{-
  30373
  25512
  65332
  33549
  35390
 -}
as :: Array (Int,Int) Int
as = listArray ((0,0),(4,4)) [3,0,3,7,3,2,5,5,1,2,6,5,3,3,2,3,3,5,4,9,3,5,3,9,0]

main :: IO ()
main = hspec $ do
  describe "Day08" $ do
    context "perimeter" $ do
      it "16 trees are visible on the perimeter" $
        countPerimeter as `shouldBe` 16
    context "isLeft" $ do
      it "row 25512 the first 5 is visible from left" $
        isLeft (1,1) as `shouldBe` True
      it "row 25512 the second 5 is not visible from left" $
        isLeft (1,2) as `shouldBe` False
      it "row 25512 the 1 is not visible from left" $
        isLeft (1,3) as `shouldBe` False
      it "row 25512 has 1 visible: 5XX from left" $
        sum [1| c <- [1..3], isLeft (1,c) as] `shouldBe` 1
      it "row 65332 the 3 is not visible" $
        isLeft (2,2) as `shouldBe` False
      it "row 33549 the 5 is visible from left" $
        isLeft (3,2) as `shouldBe` True
      it "row 33549 the second 3 is not visible" $
        isLeft (3,1) as `shouldBe` False
      it "row 33549 the 4 is not visible" $
        isLeft (3,3) as `shouldBe` False
    context "isRight" $ do
      it "row 25512 the second 5 is visible from right" $
        isRight (1,2) as `shouldBe` True
      it "row 25512 the first 5 is not visible from right" $
        isRight (1,1) as `shouldBe` False
      it "row 25512 the 1 is not visible from right" $
        isLeft (1,3) as `shouldBe` False
      it "row 65332 the 5 is visible from right" $
        isRight (2,1) as `shouldBe` True
      it "row 65332 the 3 is not visible" $
        isRight (2,2) as `shouldBe` False
      it "row 33549 the second 3 is not visible" $
        isRight (3,1) as `shouldBe` False
      it "row 33549 the 5 is not visible from right" $
        isRight (3,2) as `shouldBe` False
      it "row 33549 the 4 is not visible" $
        isRight (3,3) as `shouldBe` False
    context "isTop" $ do
      it "row 25512 the first 5 is visible from top" $
        isTop (1,1) as `shouldBe` True
      it "row 25512 the second 5 is visible from top" $
        isTop (1,2) as `shouldBe` True
      it "row 25512 the 1 is not visible from top" $
        isTop (1,3) as `shouldBe` False
      it "row 65332 the 3 is not visible" $
        isTop (2,2) as `shouldBe` False
      it "row 33549 the 5 is not visible from top" $
        isTop (3,2) as `shouldBe` False
      it "row 33549 the 4 is not visible from top" $
        isTop (3,3) as `shouldBe` False
    context "isBot" $ do
      it "row 33549 the 5 is visible from bottom" $
        isBot (3,2) as `shouldBe` True
      it "row 33549 the 4 is not visible from bottom" $
        isBot (3,3) as `shouldBe` False
      it "row 33549 the second 3 is not visible" $
        isBot (3,1) as `shouldBe` False
      it "row 65332 the 3 is not visible" $
        isBot (2,2) as `shouldBe` False
    context "part 1" $
      it "expect 21: 16 perimeter + 5 visible" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 21
    context "part 2" $
      it "expect unit" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` ()
