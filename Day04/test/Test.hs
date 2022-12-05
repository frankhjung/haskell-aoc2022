{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Text             (Text)
import           Solve                 (Record (..), contains, count, overlaps,
                                        rangeParser, recordParser, solve,
                                        solve2)
import           Test.Hspec            (context, describe, hspec, it, shouldBe)
import           Test.Hspec.Attoparsec (shouldFailOn, shouldSucceedOn)

main :: IO ()
main = hspec $ do
  describe "Day04" $ do
    context "range parser" $ do
      it "2-5 parses as Range (2,5)" $
        rangeParser `shouldSucceedOn` ("2-5,3-4\n" :: Text)
      it "2,5 fails" $
        rangeParser `shouldFailOn` ("2,5\n" :: Text)
    context "record parser" $ do
      it "2-5,3-4 parses as Record (2,5) (3,4)" $
        recordParser `shouldSucceedOn` ("2-5,3-4\n" :: Text)
      it "2-5-3-4 fails" $
        recordParser `shouldFailOn` ("2-5-3-4" :: Text)
    context "contains" $ do
      it "(2,5) contains (3,4)" $
        contains (Record (2,5) (3,4))
      it "(3,4) is contained in (2,5)" $
        contains (Record (3,4) (2,5))
      it "(2,8) contains (3,7)" $
        contains (Record (2,8) (3,7))
      it "(6,6) is contained in (4,6)" $
        contains (Record (6,6) (4,6))
    context "not contains" $ do
      it "(2,5) does not contain (5,6)" $
        not $ contains (Record (2,5) (5,6))
      it "(5,6) does not contain (2,5)" $
        not $ contains (Record (5,6) (2,5))
      it "(2,4) does not contain (6,8)" $
        not $ contains (Record (2,4) (6,8))
    context "count" $ do
      it "[True,True,False] is 2" $
        count [True,True,False] `shouldBe` 2
      it "[False,False,False] is 0" $
        count [False,False,False] `shouldBe` 0
    context "part 1" $
      it "expected 2" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 2
    -- 5-7,7-9 overlaps in a single section, 7.
    context "overlaps" $ do
      it "(5,7) overlaps (7,9)" $
        overlaps (Record (5,7) (7,9))
    -- 2-8,3-7 overlaps all of the sections 3 through 7.
    context "overlaps" $ do
      it "(2,8) overlaps (3,7)" $
        overlaps (Record (2,8) (3,7))
    -- 6-6,4-6 overlaps in a single section, 6.
    context "overlaps" $ do
      it "(6,6) overlaps (4,6)" $
        overlaps (Record (6,6) (4,6))
    -- 2-6,4-8 overlaps in sections 4, 5, and 6.
    context "overlaps" $ do
      it "(2,6) overlaps (4,8)" $
        overlaps (Record (2,6) (4,8))
    -- 2-4,6-8 does not overlap
    context "not overlapping" $ do
      it "(2,4) does not overlap (6,8)" $
        not $ overlaps (Record (2,4) (6,8))
    -- 2-3,4-5 does not overlap
    context "not overlapping" $ do
      it "(2,3) does not overlap (4,5)" $
        not $ overlaps (Record (2,3) (4,5))
    context "part 2" $
      it "expected 4" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` 4
