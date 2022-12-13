module Main (main) where

import           Data.Set   as S (fromList)
import           Solve      (Position (..), move, moveHeadTail, solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day09" $ do

    context "move" $ do
      it "expect initial state" $ do
        let q = Position (0,0) (0,0) (S.fromList [(0,0)])
        move [(0,0)] `shouldBe` q
      it "expect head moves but not tail" $ do
        let q = Position (0,0) (0,0) (S.fromList [(0,0)])
        move [(1,0),(-1,0)] `shouldBe` q
      it "expect head moves but not tail" $ do
        let q = Position (0,0) (0,0) (S.fromList [(0,0)])
        move [(1,0),(0,1),(-1,0),(0,-1)] `shouldBe` q
      it "expect head move" $ do
        let q = Position (1,0) (0,0) (S.fromList [(0,0)])
        move [(1,0)] `shouldBe` q
      it "expect tail move" $ do
        let q = Position (2,0) (1,0) (S.fromList [(0,0),(1,0)])
        move [(1,0),(1,0)] `shouldBe` q
      it "expect tail move right" $ do
        let q = Position (4,0) (3,0) (S.fromList [(0,0),(1,0),(2,0),(3,0)])
        move (replicate 4 (1,0)) `shouldBe` q
      it "expect tail move up" $ do
        let q = Position (0,4) (0,3) (S.fromList [(0,0),(0,1),(0,2),(0,3)])
        move (replicate 4 (0,1)) `shouldBe` q

    context "moveHeadTail right" $ do
      it "expect no move" $ do
        let p = Position (1,2) (1,2) (S.fromList [(1,2)])
        moveHeadTail p (0,0) `shouldBe` p
      it "expect head move right" $ do
        let p = Position (0,0) (0,0) (S.fromList [(0,0)])
            q = Position (1,0) (0,0) (S.fromList [(0,0)])
        moveHeadTail p (1,0) `shouldBe` q
      it "expect tail move right" $ do
        let p = Position (1,0) (0,0) (S.fromList [(0,0)])
            q = Position (2,0) (1,0) (S.fromList [(0,0),(1,0)])
        moveHeadTail p (1,0) `shouldBe` q

    context "moveHeadTail left" $ do
      it "expect head move left" $ do
        let p = Position (0,0) (0,0) (S.fromList [(0,0)])
            q = Position (-1,0) (0,0) (S.fromList [(0,0)])
        moveHeadTail p (-1,0) `shouldBe` q
      it "expect tail move left" $ do
        let p = Position (-1,0) (0,0) (S.fromList [(0,0)])
            q = Position (-2,0) (-1,0) (S.fromList [(-1,0),(0,0)])
        moveHeadTail p (-1,0) `shouldBe` q

    context "moveHeadTail diagonal" $ do
      it "expect diagonal up right" $ do
        let q = Position (1,2) (1,1) (S.fromList [(0,0),(1,1)])
        move [(1,0),(0,1),(0,1)] `shouldBe` q
      it "expect diagonal up left" $ do
        let q = Position (-1,2) (-1,1) (S.fromList [(0,0),(-1,1)])
        move [(-1,0),(0,1),(0,1)] `shouldBe` q
      it "expect diagonal down right" $ do
        let q = Position (1,-2) (1,-1) (S.fromList [(0,0),(1,-1)])
        move [(1,0),(0,-1),(0,-1)] `shouldBe` q
      it "expect diagonal down left" $ do
        let q = Position (-1,-2) (-1,-1) (S.fromList [(0,0),(-1,-1)])
        move [(0,-1),(-1,0),(0,-1)] `shouldBe` q

    context "part 1" $
      it "expect 13" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 13

    context "part 2" $
      it "expect unit" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` ()
